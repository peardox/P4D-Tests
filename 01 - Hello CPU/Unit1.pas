unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Zip,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  PythonEngine, PyCommon, PyModule, PyEnvironment.Embeddable.Res,
  PyPackage, OpenCV, PyEnvironment, PyEnvironment.Embeddable, PyUtils,
  Vcl.PythonGUIInputOutput, Vcl.StdCtrls, System.Threading, Vcl.Buttons,
  PSUtil, PyEnvironment.Embeddable.Res.Python39, Boto3, Vcl.ExtCtrls, PyTorch;

type
  TForm1 = class(TForm)
    mmLog: TMemo;
    PyEngine: TPythonEngine;
    PythonGUIInputOutput1: TPythonGUIInputOutput;
    OpenDialog1: TOpenDialog;
    PSUtil: TPSUtil;
    PyEmbeddedResEnvironment391: TPyEmbeddedResEnvironment39;
    Panel1: TPanel;
    Setup: TButton;
    btnTest: TBitBtn;
    Memo1: TMemo;
    Torch: TPyTorch;
    procedure PyEmbeddedResEnvironment391AfterSetup(Sender: TObject;
      const APythonVersion: string);
    procedure PyEmbeddedResEnvironment391BeforeSetup(Sender: TObject;
      const APythonVersion: string);
    procedure PyModuleAfterInstall(Sender: TObject);
    procedure PyModuleBeforeInstall(Sender: TObject);
    procedure PyModuleInstallError(Sender: TObject; AErrorMessage: string);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure SetupClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnTestClick(Sender: TObject);
    procedure PyEmbeddedResEnvironment391ZipProgress(Sender: TObject;
      ADistribution: TPyCustomEmbeddableDistribution; FileName: string;
      Header: TZipHeader; Position: Int64);
    procedure PyEmbeddedResEnvironment391AfterActivate(Sender: TObject;
      const APythonVersion: string; const AActivated: Boolean);
    procedure PyEmbeddedResEnvironment391BeforeActivate(Sender: TObject;
      const APythonVersion: string);
    procedure PyEmbeddedResEnvironment391Ready(Sender: TObject;
      const APythonVersion: string);
  private
    FTask: ITask;
    function IsTaskRunning(): boolean;
    function Process(const AImagePath: string): variant;
    { Déclarations privées }
    procedure Log(const AMsg: string);
    procedure Log2(const AMsg: string);
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

uses
  PyPackage.Manager.Pip,
  PyPackage.Manager.Defs.Pip,
  System.JSON.Readers,
  System.JSON.Types,
  System.IOUtils;

{$R *.dfm}

procedure TForm1.SetupClick(Sender: TObject);
begin
  Setup.Enabled := false;
  btnTest.Enabled := false;

  FTask := TTask.Run(procedure() begin
    try
      PyEmbeddedResEnvironment391.Setup(PyEmbeddedResEnvironment391.PythonVersion);
      FTask.CheckCanceled();
      TThread.Synchronize(nil, procedure() begin
        var act: Boolean := PyEmbeddedResEnvironment391.Activate(PyEmbeddedResEnvironment391.PythonVersion);
        if act then
          Log('Activated')
        else
          Log('Activation failed');
      end);

      FTask.CheckCanceled();
      PSUtil.Install();

      var popts: TPyPackageManagerDefsPip;
      popts := TPyPackageManagerDefsPip(Torch.Managers.Pip);
      popts.InstallOptions.ExtraIndexUrl := 'https://download.pytorch.org/whl/cu116';
      Torch.Install();

      FTask.CheckCanceled();
      TThread.Queue(nil, procedure() begin
        try
          MaskFPUExceptions(true);
          try
            PSUtil.Import();
            Torch.Import();
          finally
            MaskFPUExceptions(false);
          end;
        finally
          Setup.Enabled := true;
          btnTest.Enabled := true;
        end;
        Log('All done!');
      end);
    except
      on E: Exception do begin
        TThread.Queue(nil, procedure() begin
          Setup.Enabled := true;
          btnTest.Enabled := true;
        end);
      end;
    end;
  end);
end;

procedure TForm1.btnTestClick(Sender: TObject);
var
  I: Integer;
begin
      var gpu_count: Variant := Torch.torch.cuda.device_count();
      var cpu_cores: Variant := PSUtil.psutil.cpu_count(False);
      var cpu_threads: Variant := PSUtil.psutil.cpu_count(True);
      var cpu_freq: Variant := PSUtil.psutil.cpu_freq();
      var virtual_memory: Variant := PSUtil.psutil.virtual_memory();

      Log2('Torch returned gpu_count = ' + gpu_count);
      if gpu_count > 0 then
        begin
          for I := 0 to gpu_count - 1 do
            begin
              var gpu_props: Variant := Torch.torch.cuda.get_device_properties(i);

              Log2('Torch returned Name = ' + gpu_props.name);
              Log2('Torch returned CudaMajor = ' + gpu_props.major);
              Log2('Torch returned CudaMajor = ' + gpu_props.minor);
              Log2('Torch returned Memory = ' + gpu_props.total_memory);
              Log2('Torch returned CUs = ' + gpu_props.multi_processor_count);
            end;
        end;
      Log2('PSUtil returned cpu_cores = ' + cpu_cores);
      Log2('PSUtil returned cpu_threads = ' + cpu_threads);
      Log2('PSUtil returned cpu_freq = ' + cpu_freq.current);
      Log2('PSUtil returned total_memory = ' + virtual_memory.total);
      Log2('PSUtil returned available_memory = ' + virtual_memory.available);
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if IsTaskRunning() then begin
    ShowMessage('Waiting for operations...');
    FTask.Cancel();
    while IsTaskRunning() do begin
      FTask.Wait(100);
      Application.ProcessMessages();
    end;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Caption := 'HelloCPU';
  Memo1.Clear;
  SetupClick(Sender);
end;

function TForm1.IsTaskRunning: boolean;
begin
  if Assigned(FTask) then
    Result := not (FTask.Status in [TTaskStatus.Completed, TTaskStatus.Exception])
  else
    Result := false;
end;

procedure TForm1.Log2(const AMsg: string);
begin
  if TThread.CurrentThread.ThreadID <> MainThreadID then
    TThread.Synchronize(nil, procedure() begin
      Memo1.Lines.Add('-> ' + AMsg);
    end)
  else
    Memo1.Lines.Add('-> ' + AMsg);
end;

procedure TForm1.Log(const AMsg: string);
begin
  if TThread.CurrentThread.ThreadID <> MainThreadID then
    TThread.Synchronize(nil, procedure() begin
      mmLog.Lines.Add('-> ' + AMsg);
    end)
  else
    mmLog.Lines.Add('-> ' + AMsg);
end;

function TForm1.Process(const AImagePath: string): variant;
begin
end;

procedure TForm1.PyEmbeddedResEnvironment391AfterActivate(Sender: TObject;
  const APythonVersion: string; const AActivated: Boolean);
begin
  Log('After activate');
end;

procedure TForm1.PyEmbeddedResEnvironment391AfterSetup(Sender: TObject;
  const APythonVersion: string);
begin
  Log(Format('Python%s has been successfully installed.', [APythonVersion]));
end;

procedure TForm1.PyEmbeddedResEnvironment391BeforeActivate(Sender: TObject;
  const APythonVersion: string);
begin
  Log('Before activate');
end;

procedure TForm1.PyEmbeddedResEnvironment391BeforeSetup(Sender: TObject;
  const APythonVersion: string);
begin
  Log(Format('Installing Python%s...', [APythonVersion]));
end;

procedure TForm1.PyEmbeddedResEnvironment391Ready(Sender: TObject;
  const APythonVersion: string);
begin
  Log('Ready');
end;

procedure TForm1.PyEmbeddedResEnvironment391ZipProgress(Sender: TObject;
  ADistribution: TPyCustomEmbeddableDistribution; FileName: string;
  Header: TZipHeader; Position: Int64);
begin
//  Log('Installing... ' + IntToStr(Position));
end;

procedure TForm1.PyModuleAfterInstall(Sender: TObject);
begin
  Log(Format('%s has been installed.', [TPyPackage(Sender).PyModuleName]));
end;

procedure TForm1.PyModuleBeforeInstall(Sender: TObject);
begin
  Log(Format('Installing %s...', [TPyPackage(Sender).PyModuleName]));
end;

procedure TForm1.PyModuleInstallError(Sender: TObject; AErrorMessage: string);
begin
  Log(Format('%s installation failed.' + #13#10 + '%s', [
    TPyPackage(Sender).PyModuleName,
    AErrorMessage]));
end;

end.
