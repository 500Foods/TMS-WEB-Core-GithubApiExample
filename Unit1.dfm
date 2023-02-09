object Form1: TForm1
  Width = 640
  Height = 555
  Color = clBlack
  CSSLibrary = cssBootstrap
  ElementClassName = 'vw-100 vh-100 d-flex p-2 bg-black'
  ElementFont = efCSS
  ElementPosition = epIgnore
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Tahoma'
  Font.Style = []
  ParentFont = False
  OnCreate = WebFormCreate
  OnResize = WebFormResize
  OnShow = WebFormShow
  object divMain: TWebHTMLDiv
    Left = 40
    Top = 88
    Width = 561
    Height = 401
    ElementClassName = 
      'd-flex flex-fill rounded border border-secondary border-2 p-2 fl' +
      'ex-column overflow-hidden gap-1'
    ElementID = 'divMain'
    HeightStyle = ssAuto
    WidthStyle = ssAuto
    ElementPosition = epIgnore
    ElementFont = efCSS
    Role = ''
    object WebEdit1: TWebEdit
      Left = 152
      Top = 24
      Width = 257
      Height = 40
      Alignment = taCenter
      ElementClassName = 'w-100 p-2 rounded m-auto border border-secondary border-2 '
      ElementID = 'WebEdit1'
      ElementFont = efCSS
      ElementPosition = epIgnore
      HeightPercent = 100.000000000000000000
      TextHint = 'Please Enter a GitHub Personal Access Token and Hit Enter.'
      Visible = False
      WidthStyle = ssAuto
      WidthPercent = 100.000000000000000000
      OnKeyDown = WebEdit1KeyDown
    end
    object divTabulator: TWebHTMLDiv
      Left = 72
      Top = 70
      Width = 441
      Height = 147
      ElementClassName = 'flex-fill d-none h-100 order-0'
      ElementID = 'divTabulator'
      HeightStyle = ssAuto
      WidthStyle = ssAuto
      ChildOrder = 1
      ElementPosition = epIgnore
      ElementFont = efCSS
      Role = ''
      Visible = False
    end
    object divChart: TWebHTMLDiv
      Left = 72
      Top = 223
      Width = 441
      Height = 122
      ElementClassName = 'calendar overflow-hidden  d-none flex-fill h-50 w-100 order-1 '
      ElementID = 'divChart'
      HeightStyle = ssAuto
      WidthStyle = ssAuto
      ChildOrder = 2
      ElementPosition = epIgnore
      ElementFont = efCSS
      Role = ''
    end
  end
  object WebTimer1: TWebTimer
    Interval = 1800000
    OnTimer = WebTimer1Timer
    Left = 104
    Top = 40
  end
end
