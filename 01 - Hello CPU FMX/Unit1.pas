unit Unit1;

interface
{$IFDEF MSWINDOWS}
 {$DEFINE USETORCH}
{$ENDIF}

uses
  System.SysUtils, System.Threading, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, PyTorch,
  PyCommon, PyModule, PyPackage, PSUtil, PyEnvironment,
  PyEnvironment.Embeddable, PyEnvironment.Embeddable.Res,
  PyEnvironment.Embeddable.Res.Python39, PythonEngine, FMX.PythonGUIInputOutput;

type
  TForm1 = class(TForm)
    PythonEngine1: TPythonEngine;
    PythonGUIInputOutput1: TPythonGUIInputOutput;
    PyEmbeddedResEnvironment391: TPyEmbeddedResEnvironment39;
    PSUtil: TPSUtil;
    Torch: TPyTorch;
    mmLog: TMemo;
    Panel1: TPanel;
    Setup: TButton;
    btnTest: TButton;
    procedure SetupClick(Sender: TObject);
    procedure btnTestClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PyEmbeddedResEnvironment391AfterSetup(Sender: TObject;
      const APythonVersion: string);
    procedure PyEmbeddedResEnvironment391BeforeSetup(Sender: TObject;
      const APythonVersion: string);
    procedure PyEmbeddedResEnvironment391Ready(Sender: TObject;
      const APythonVersion: string);
    procedure PyModuleAfterInstall(Sender: TObject);
    procedure PyModuleBeforeInstall(Sender: TObject);
    procedure PyModuleInstallError(Sender: TObject; AErrorMessage: string);
  private
    { Private declarations }
    FTask: ITask;
    function IsTaskRunning(): boolean;
    procedure Log(const AMsg: string);
  public
    { Public declarations }
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


{$R *.fmx}

procedure TForm1.FormCreate(Sender: TObject);
begin
  PythonEngine1.UseLastKnownVersion := False;
  PyEmbeddedResEnvironment391.EnvironmentPath := 'python';
  Caption := 'HelloCPUFMX';
  mmLog.Lines.Clear;
end;

function TForm1.IsTaskRunning: boolean;
begin
  if Assigned(FTask) then
    Result := not (FTask.Status in [TTaskStatus.Completed, TTaskStatus.Exception])
  else
    Result := false;
end;

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

      {$IFDEF USETORCH}
      FTask.CheckCanceled();
      {$IFDEF MSWINDOWS}
      var popts: TPyPackageManagerDefsPip;
      popts := TPyPackageManagerDefsPip(Torch.Managers.Pip);
      popts.InstallOptions.ExtraIndexUrl := 'https://download.pytorch.org/whl/cu116';
      {$ENDIF}
      Torch.Install();
      {$ENDIF}

      FTask.CheckCanceled();
      PSUtil.Install();

      FTask.CheckCanceled();
      TThread.Queue(nil, procedure() begin
        try
          {$IFDEF MSWINDOWS}
          MaskFPUExceptions(true);
          {$ENDIF}
          try
            try
              {$IFDEF USETORCH}
              Log('Importing Torch');
              Torch.Import();
              {$ENDIF}
              Log('Importing PSUtil');
              PSUtil.Import();
            except
            on E: Exception do begin
              TThread.Queue(nil, procedure() begin
                Setup.Enabled := false;
                btnTest.Enabled := false;
                Log('An IMPORTexception was caught');
                Log('Class : ' + E.ClassName);
                Log('Error : ' + E.Message);
              end);
              end;
            end;
          finally
            {$IFDEF MSWINDOWS}
            MaskFPUExceptions(false);
            {$ENDIF}
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
          Setup.Enabled := false;
          btnTest.Enabled := false;
          Log('An INSTALL exception was caught');
          Log('Class : ' + E.ClassName);
          Log('Error : ' + E.Message);
        end);
      end;
    end;
  end);
end;

procedure TForm1.btnTestClick(Sender: TObject);
{$IFDEF USETORCH}
var
  I: Integer;
{$ENDIF}
begin
  if PSUtil.IsInstalled then
    begin
      if PSUtil.IsImported then
        begin
          var cpu_cores: Variant := PSUtil.psutil.cpu_count(False);
          var cpu_threads: Variant := PSUtil.psutil.cpu_count(True);
          var cpu_freq: Variant := PSUtil.psutil.cpu_freq();
          var virtual_memory: Variant := PSUtil.psutil.virtual_memory();

          {$IFDEF USETORCH}
          var gpu_count: Variant := Torch.torch.cuda.device_count();
          Log('Torch returned gpu_count = ' + gpu_count);
          if gpu_count > 0 then
            begin
              for I := 0 to gpu_count - 1 do
                begin
                  var gpu_props: Variant := Torch.torch.cuda.get_device_properties(i);

                  Log('Torch returned Name = ' + gpu_props.name);
                  Log('Torch returned CudaMajor = ' + gpu_props.major);
                  Log('Torch returned CudaMajor = ' + gpu_props.minor);
                  Log('Torch returned Memory = ' + gpu_props.total_memory);
                  Log('Torch returned CUs = ' + gpu_props.multi_processor_count);
                end;
            end;
          {$ENDIF}
          Log('PSUtil returned cpu_cores = ' + cpu_cores);
          Log('PSUtil returned cpu_threads = ' + cpu_threads);
          Log('PSUtil returned cpu_freq = ' + cpu_freq.current);
          Log('PSUtil returned total_memory = ' + virtual_memory.total);
          Log('PSUtil returned available_memory = ' + virtual_memory.available);
        end
      else
        Log('PSUttil did not import correctly');
    end
  else
    Log('PSUttil did not install correctly');
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


procedure TForm1.PyEmbeddedResEnvironment391AfterSetup(Sender: TObject;
  const APythonVersion: string);
begin
  Log(Format('Python%s has been successfully installed.', [APythonVersion]));
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
