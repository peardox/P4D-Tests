unit Unit1;

interface
{$IFDEF MSWINDOWS}
  {$DEFINE USETORCHVISION}
  {$DEFINE USETORCH}
  {$DEFINE USEPSUTIL}
  {$DEFINE USENUMPY}
  {$DEFINE USEONNXRUNTIME}
  {$DEFINE USEBOTO3}
  {$DEFINE USEREMBG}
  {$DEFINE USESCIPY}
  {$DEFINE USEPILLOW}
{$ENDIF}
{$IFDEF MACOS}
  {$DEFINE USETORCHVISION}
  {$DEFINE USETORCH}
//  {$DEFINE USEPSUTIL}
  {$DEFINE USENUMPY}
  {$DEFINE USEONNXRUNTIME}
  {$DEFINE USEBOTO3}
  {$DEFINE USEREMBG}
  {$DEFINE USESCIPY}
  {$DEFINE USEPILLOW}
{$ENDIF}
{$IFDEF ANDROID}
//  {$DEFINE USETORCHVISION}
//  {$DEFINE USETORCH}
//  {$DEFINE USEPSUTIL}
//  {$DEFINE USENUMPY}
//  {$DEFINE USEONNXRUNTIME}
  {$DEFINE USEBOTO3}
//  {$DEFINE USEREMBG}
//  {$DEFINE USESCIPY}
//  {$DEFINE USEPILLOW}
{$ENDIF}

uses
  System.SysUtils, System.Threading, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  PyTorch,
  PSUtil,
  PyCommon, PyModule, PyPackage, PyEnvironment,
  PyEnvironment.Embeddable, PyEnvironment.Embeddable.Res,
  PyEnvironment.Embeddable.Res.Python39, PythonEngine, FMX.PythonGUIInputOutput,
  NumPy, PyEnvironment.AddOn, PyEnvironment.AddOn.GetPip, SciPy, Pillow, RemBG,
  Boto3, ONNXRuntime, TorchVision;

type
  TForm1 = class(TForm)
    PythonEngine1: TPythonEngine;
    PythonGUIInputOutput1: TPythonGUIInputOutput;
    Torch: TPyTorch;
    mmLog: TMemo;
    Panel1: TPanel;
    Setup: TButton;
    btnTest: TButton;
    NumPy: TNumPy;
    PyEmbeddedResEnvironment391: TPyEmbeddedResEnvironment39;
    PyEnvironmentAddOnGetPip1: TPyEnvironmentAddOnGetPip;
    ONNXRuntime: TONNXRuntime;
    Boto3: TBoto3;
    RemBG: TRemBG;
    Pillow: TPillow;
    SciPy: TSciPy;
    TorchVision: TTorchVision;
    PSUtil: TPSUtil;
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
    procedure Log(const AMsg: string; const Prefix: String = '');
    procedure ListPythonFiles(const PythonDir: String; const Depth: Cardinal = 0);
  public
    { Public declarations }
    EnvPath: String;
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
  {$IFDEF ANDROID}
  EnvPath := IncludeTrailingPathDelimiter(System.IOUtils.TPath.GetDocumentsPath);
  {$ELSE}
  EnvPath := '';
  {$ENDIF}

  PyEmbeddedResEnvironment391.EnvironmentPath := EnvPath + 'python';
  Caption := 'HelloCPUFMX';
  mmLog.Lines.Clear;
  Log('Environment Path = ' + PyEmbeddedResEnvironment391.EnvironmentPath);
  SetupClick(Self);
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
          begin
            Log('Activated');
            var Shim: TStringList;
            Shim := Nil;
            Shim := TStringList.Create;
            Shim.Add('import sys');
            Shim.Add('import pip');
            Shim.Add('print("pip =", pip.__version__)');
            Shim.Add('for p in sys.path:');
            Shim.Add('  print(p)');
            Log('Executiing the following Python to check everything works');
            Log('=========================================================');

            for var i := 0 to Shim.Count - 1 do
              Log(Shim[i]);

            Log('=========================================================');

            PythonEngine1.ExecStrings(Shim);
            Log('=========================================================');

            Shim.Free;
          end

        else
          Log('Activation failed');
      end);

      FTask.CheckCanceled();

      {$IFDEF USEPILLOW}
      Log('Attempting to Install Pillow');
      try
        Pillow.Install();
      except
      on E: Exception do begin
        TThread.Queue(nil, procedure() begin
          Setup.Enabled := false;
          btnTest.Enabled := false;
          Log('An IMPORT exception was caught');
          Log('Class : ' + E.ClassName);
          Log('Error : ' + E.Message);
        end);
        end;
      end;
      FTask.CheckCanceled();
      {$ENDIF}

      {$IFDEF USESCIPY}
      Log('Attempting to Install SciPy');
      try
        SciPy.Install();
      except
      on E: Exception do begin
        TThread.Queue(nil, procedure() begin
          Setup.Enabled := false;
          btnTest.Enabled := false;
          Log('An IMPORT exception was caught');
          Log('Class : ' + E.ClassName);
          Log('Error : ' + E.Message);
        end);
        end;
      end;
      FTask.CheckCanceled();
      {$ENDIF}

      {$IFDEF USEREMBG}
      Log('Attempting to Install RemBG');
      try
        RemBG.Install();
      except
      on E: Exception do begin
        TThread.Queue(nil, procedure() begin
          Setup.Enabled := false;
          btnTest.Enabled := false;
          Log('An IMPORT exception was caught');
          Log('Class : ' + E.ClassName);
          Log('Error : ' + E.Message);
        end);
        end;
      end;
      FTask.CheckCanceled();
      {$ENDIF}

      {$IFDEF USEBOTO3}
      Log('Attempting to Install Boto3');
      try
        Boto3.Install();
      except
      on E: Exception do begin
        TThread.Queue(nil, procedure() begin
          Setup.Enabled := false;
          btnTest.Enabled := false;
          Log('An IMPORT exception was caught');
          Log('Class : ' + E.ClassName);
          Log('Error : ' + E.Message);
        end);
        end;
      end;
      FTask.CheckCanceled();
      {$ENDIF}

      {$IFDEF USEONNXRUNTIME}
      Log('Attempting to Install ONNXRuntime');
      try
        ONNXRuntime.Install();
      except
      on E: Exception do begin
        TThread.Queue(nil, procedure() begin
          Setup.Enabled := false;
          btnTest.Enabled := false;
          Log('An IMPORT exception was caught');
          Log('Class : ' + E.ClassName);
          Log('Error : ' + E.Message);
        end);
        end;
      end;
      FTask.CheckCanceled();
      {$ENDIF}

      {$IFDEF USENUMPY}
      Log('Attempting to Install NumPy');
      try
        NumPy.Install();
      except
      on E: Exception do begin
        TThread.Queue(nil, procedure() begin
          Setup.Enabled := false;
          btnTest.Enabled := false;
          Log('An IMPORT exception was caught');
          Log('Class : ' + E.ClassName);
          Log('Error : ' + E.Message);
        end);
        end;
      end;
      FTask.CheckCanceled();
      {$ENDIF}

      {$IFDEF USETORCH}
      Log('Attempting to Install Torch');
      {$IFDEF CPUX64}
      MaskFPUExceptions(true);
      {$ENDIF}
      try
       Torch.Install();
      except
      on E: Exception do begin
        TThread.Queue(nil, procedure() begin
          Setup.Enabled := false;
          btnTest.Enabled := false;
          Log('An IMPORT exception was caught');
          Log('Class : ' + E.ClassName);
          Log('Error : ' + E.Message);
        end);
        end;
      end;
      {$IFDEF CPUX64}
      MaskFPUExceptions(false);
      {$ENDIF}
      Log('Finished Installing Torch');
      FTask.CheckCanceled();
      {$ENDIF}

      {$IFDEF USETORCHVISION}
      Log('Attempting to Install TorchVision');
      {$IFDEF CPUX64}
      MaskFPUExceptions(true);
      {$ENDIF}
      try
       TorchVision.Install();
      except
      on E: Exception do begin
        TThread.Queue(nil, procedure() begin
          Setup.Enabled := false;
          btnTest.Enabled := false;
          Log('An IMPORT exception was caught');
          Log('Class : ' + E.ClassName);
          Log('Error : ' + E.Message);
        end);
        end;
      end;
      {$IFDEF CPUX64}
      MaskFPUExceptions(false);
      {$ENDIF}
      Log('Finished Installing Torch');
      FTask.CheckCanceled();
      {$ENDIF}

      {$IFDEF USEPSUTIL}
      Log('Attempting to Install PSUtil');
      try
        PSUtil.Install();
      except
      on E: Exception do begin
        TThread.Queue(nil, procedure() begin
          Setup.Enabled := false;
          btnTest.Enabled := false;
          Log('An IMPORT exception was caught');
          Log('Class : ' + E.ClassName);
          Log('Error : ' + E.Message);
        end);
        end;
      end;
      Log('Finished Installing PSUtil');
      FTask.CheckCanceled();
      {$ENDIF}

      TThread.Queue(nil, procedure() begin
        try
          try
            try
              {$IFDEF USENUMPY}
              Log('Importing NumPy');
              NumPy.Import();
              {$ENDIF}
              {$IFDEF USETORCH}
              Log('Importing Torch');
              {$IFDEF CPUX64}
              MaskFPUExceptions(true);
              {$ENDIF}
              Torch.Import();
              {$ENDIF}
              {$IFDEF USEPSUTIL}
              Log('Importing PSUtil');
              PSUtil.Import();
              {$ENDIF}
            except
            on E: Exception do begin
              TThread.Queue(nil, procedure() begin
                Setup.Enabled := false;
                btnTest.Enabled := false;
                Log('An IMPORT exception was caught');
                Log('Class : ' + E.ClassName);
                Log('Error : ' + E.Message);
              end);
              end;
            end;
          finally
            {$IFDEF CPUX64}
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
  {$IFDEF USEPSUTIL}
  var cpu_cores: Variant := PSUtil.psutil.cpu_count(False);
  var cpu_threads: Variant := PSUtil.psutil.cpu_count(True);
  var virtual_memory: Variant := PSUtil.psutil.virtual_memory();
  {$ENDIF}

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
  {$IFDEF USEPSUTIL}
  Log('PSUtil returned cpu_cores = ' + cpu_cores);
  Log('PSUtil returned cpu_threads = ' + cpu_threads);
  Log('PSUtil returned total_memory = ' + virtual_memory.total);
  Log('PSUtil returned available_memory = ' + virtual_memory.available);
  {$ENDIF}
end;

procedure TForm1.Log(const AMsg: string; const Prefix: String = '');
begin
  if TThread.CurrentThread.ThreadID <> MainThreadID then
    TThread.Synchronize(nil, procedure() begin
      mmLog.Lines.Add(Prefix + AMsg);
      mmLog.GoToTextEnd;
      mmLog.Repaint;
    end)
  else
    begin
      mmLog.Lines.Add(Prefix + AMsg);
      mmLog.GoToTextEnd;
      mmLog.Repaint;
    end;
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

procedure TForm1.ListPythonFiles(const PythonDir: String; const Depth: Cardinal = 0);
var
  SearchRec: TSearchRec;
  filespec: String;
  FileName: String;
  spacer: String;
begin
  filespec := IncludeTrailingPathDelimiter(PythonDir);

  {$ifdef MSWINDOWS}
  filespec := filespec + '*.*';
  {$ELSE}
  filespec := filespec + '*';
  {$ENDIF}

  spacer := StringOfChar(' ', Depth * 2);

  if (FindFirst(filespec, faAnyFile, SearchRec) = 0) then
    begin
      repeat
        FileName := SearchRec.Name;
        if ((SearchRec.Attr and faDirectory) = 0) then
          begin
            Log(spacer + FileName);
          end
        else
          begin
            if (FileName <> '.') and (FileName <> '..') then
              begin
                Log(spacer + FileName);
                ListPythonFiles(PythonDir + System.IOUtils.TPath.DirectorySeparatorChar + FileName, Depth + 1);
              end;
          end;
      until FindNext(SearchRec) <> 0;
      FindClose(SearchRec);
    end;
    mmLog.Lines.SaveToFile(EnvPath + 'PythonFileList.txt');
end;

end.
