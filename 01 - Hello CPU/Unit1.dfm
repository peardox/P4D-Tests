object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 336
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object mmLog: TMemo
    Left = 392
    Top = 0
    Width = 243
    Height = 295
    Align = alRight
    ReadOnly = True
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 295
    Width = 635
    Height = 41
    Align = alBottom
    TabOrder = 1
    object Setup: TButton
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Setup'
      TabOrder = 0
      OnClick = SetupClick
    end
    object btnTest: TBitBtn
      Left = 89
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Test'
      TabOrder = 1
      OnClick = btnTestClick
    end
  end
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 392
    Height = 295
    Align = alClient
    Lines.Strings = (
      'Memo1')
    TabOrder = 2
  end
  object PyEngine: TPythonEngine
    AutoLoad = False
    FatalAbort = False
    FatalMsgDlg = False
    IO = PythonGUIInputOutput1
    Left = 505
    Top = 24
  end
  object PythonGUIInputOutput1: TPythonGUIInputOutput
    UnicodeIO = True
    RawOutput = False
    Output = mmLog
    Left = 504
    Top = 80
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = '.png'
    Filter = 'PNG|*.png'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 504
    Top = 248
  end
  object PSUtil: TPSUtil
    PythonEngine = PyEngine
    PyEnvironment = PyEmbeddedResEnvironment391
    ManagerKind = pip
    BeforeInstall = PyModuleBeforeInstall
    OnInstallError = PyModuleInstallError
    AfterInstall = PyModuleAfterInstall
    Left = 504
    Top = 200
  end
  object PyEmbeddedResEnvironment391: TPyEmbeddedResEnvironment39
    BeforeSetup = PyEmbeddedResEnvironment391BeforeSetup
    AfterSetup = PyEmbeddedResEnvironment391AfterSetup
    BeforeActivate = PyEmbeddedResEnvironment391BeforeActivate
    AfterActivate = PyEmbeddedResEnvironment391AfterActivate
    OnReady = PyEmbeddedResEnvironment391Ready
    AutoLoad = False
    PythonVersion = '3.9'
    PythonEngine = PyEngine
    OnZipProgress = PyEmbeddedResEnvironment391ZipProgress
    EnvironmentPath = 'python'
    Left = 512
    Top = 152
  end
  object Torch: TPyTorch
    PythonEngine = PyEngine
    PyEnvironment = PyEmbeddedResEnvironment391
    ManagerKind = pip
    BeforeInstall = PyModuleBeforeInstall
    OnInstallError = PyModuleInstallError
    AfterInstall = PyModuleAfterInstall
    Left = 552
    Top = 200
  end
end
