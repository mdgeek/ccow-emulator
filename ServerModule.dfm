object RestServer: TRestServer
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Left = 634
  Top = 300
  Height = 255
  Width = 455
  object httpServer: TIdHTTPServer
    Bindings = <>
    DefaultPort = 2116
    OnAfterBind = httpServerAfterBind
    OnListenException = httpServerListenException
    OnCommandGet = httpServerCommandGet
    Left = 208
    Top = 96
  end
end
