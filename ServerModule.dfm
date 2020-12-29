object RestServer: TRestServer
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Left = 634
  Top = 300
  Height = 255
  Width = 455
  object httpServer: TIdHTTPServer
    Bindings = <
      item
        IP = '127.0.0.1'
        Port = 2116
      end>
    DefaultPort = 2116
    OnAfterBind = httpServerAfterBind
    OnCommandGet = httpServerCommandGet
    Left = 208
    Top = 96
  end
  object Timer: TTimer
    OnTimer = TimerTimer
    Left = 280
    Top = 96
  end
end
