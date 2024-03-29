unit CCOW_TLB;

// ************************************************************************ //
// WARNING
// -------
// The types declared in this file were generated from data read from a
// Type Library. If this type library is explicitly or indirectly (via
// another type library referring to this type library) re-imported, or the
// 'Refresh' command of the Type Library Editor activated while editing the
// Type Library, the contents of this file will be regenerated and all
// manual modifications will be lost.
// ************************************************************************ //

// $Rev: 98336 $
// File generated on 10/4/2022 7:27:46 AM from Type Library described below.

// ************************************************************************  //
// Type Lib: Y:\workspace\ccow-emulator\CCOWEmulator\CCOWEmulator (1)
// LIBID: {1D57E007-B996-4E6B-B9A4-2F62A2399CFF}
// LCID: 0
// Helpfile:
// HelpString: CCOWEmulator Library
// DepndLst:
//   (1) v2.0 stdole, (C:\Windows\SysWOW64\stdole2.tlb)
// SYS_KIND: SYS_WIN32
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers.
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
{$ALIGN 4}

interface

uses Winapi.Windows, System.Classes, System.Variants, System.Win.StdVCL, Vcl.Graphics, Vcl.OleServer, Winapi.ActiveX;


// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:
//   Type Libraries     : LIBID_xxxx
//   CoClasses          : CLASS_xxxx
//   DISPInterfaces     : DIID_xxxx
//   Non-DISP interfaces: IID_xxxx
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  CCOWMajorVersion = 1;
  CCOWMinorVersion = 0;

  LIBID_CCOW: TGUID = '{1D57E007-B996-4E6B-B9A4-2F62A2399CFF}';

  IID_IContextManager: TGUID = '{41126C5E-A069-11D0-808F-00A0240943E4}';
  IID_IContextData: TGUID = '{2AAE4991-A1FC-11D0-808F-00A0240943E4}';
  IID_ISecureBinding: TGUID = '{F933331D-91C6-11D2-AB9F-4471FBC00000}';
  IID_IContextParticipant: TGUID = '{3E3DD272-998E-11D0-808D-00A0240943E4}';
  IID_IContextAction: TGUID = '{32A82A2D-76E9-4C4B-9716-74538B9A37A9}';
  IID_IContextFilter: TGUID = '{637CD323-0175-45FA-AD5C-B7DE53CD1AFD}';
  IID_IContextSession: TGUID = '{A76D5873-D2D3-4668-AE0F-37C8B6A380E4}';
  IID_ISecureContextData: TGUID = '{6F530680-BC14-11D1-90B1-76C60D000000}';
  IID_IImplementationInformation: TGUID = '{41123600-6CE1-11D1-AB3F-E892F500000C}';
  CLASS_ContextManager: TGUID = '{E56E7071-E8FC-4D76-872E-10EDE51ED076}';
  IID_IAuthenticationRepository: TGUID = '{12B28736-2895-11D2-BD6E-0060B0573ADC}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary
// *********************************************************************//
  IContextManager = interface;
  IContextManagerDisp = dispinterface;
  IContextData = interface;
  IContextDataDisp = dispinterface;
  ISecureBinding = interface;
  ISecureBindingDisp = dispinterface;
  IContextParticipant = interface;
  IContextParticipantDisp = dispinterface;
  IContextAction = interface;
  IContextActionDisp = dispinterface;
  IContextFilter = interface;
  IContextFilterDisp = dispinterface;
  IContextSession = interface;
  IContextSessionDisp = dispinterface;
  ISecureContextData = interface;
  ISecureContextDataDisp = dispinterface;
  IImplementationInformation = interface;
  IImplementationInformationDisp = dispinterface;
  IAuthenticationRepository = interface;
  IAuthenticationRepositoryDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library
// (NOTE: Here we map each CoClass to its Default Interface)
// *********************************************************************//
  ContextManager = IContextManager;


// *********************************************************************//
// Interface: IContextManager
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {41126C5E-A069-11D0-808F-00A0240943E4}
// *********************************************************************//
  IContextManager = interface(IDispatch)
    ['{41126C5E-A069-11D0-808F-00A0240943E4}']
    function Get_MostRecentContextCoupon: Integer; safecall;
    function JoinCommonContext(const contextParticipant: IDispatch;
                               const sApplicationTitle: WideString; survey: WordBool; wait: WordBool): Integer; safecall;
    procedure LeaveCommonContext(participantCoupon: Integer); safecall;
    function StartContextChanges(participantCoupon: Integer): Integer; safecall;
    function EndContextChanges(contextCoupon: Integer; var someBusy: WordBool): OleVariant; safecall;
    procedure UndoContextChanges(contextCoupon: Integer); safecall;
    procedure PublishChangesDecision(contextCoupon: Integer; const decision: WideString); safecall;
    procedure SuspendParticipation(participantCoupon: Integer); safecall;
    procedure ResumeParticipation(participantCoupon: Integer; wait: WordBool); safecall;
    property MostRecentContextCoupon: Integer read Get_MostRecentContextCoupon;
  end;

// *********************************************************************//
// DispIntf:  IContextManagerDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {41126C5E-A069-11D0-808F-00A0240943E4}
// *********************************************************************//
  IContextManagerDisp = dispinterface
    ['{41126C5E-A069-11D0-808F-00A0240943E4}']
    property MostRecentContextCoupon: Integer readonly dispid 201;
    function JoinCommonContext(const contextParticipant: IDispatch;
                               const sApplicationTitle: WideString; survey: WordBool; wait: WordBool): Integer; dispid 202;
    procedure LeaveCommonContext(participantCoupon: Integer); dispid 203;
    function StartContextChanges(participantCoupon: Integer): Integer; dispid 204;
    function EndContextChanges(contextCoupon: Integer; var someBusy: WordBool): OleVariant; dispid 205;
    procedure UndoContextChanges(contextCoupon: Integer); dispid 206;
    procedure PublishChangesDecision(contextCoupon: Integer; const decision: WideString); dispid 207;
    procedure SuspendParticipation(participantCoupon: Integer); dispid 208;
    procedure ResumeParticipation(participantCoupon: Integer; wait: WordBool); dispid 209;
  end;

// *********************************************************************//
// Interface: IContextData
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2AAE4991-A1FC-11D0-808F-00A0240943E4}
// *********************************************************************//
  IContextData = interface(IDispatch)
    ['{2AAE4991-A1FC-11D0-808F-00A0240943E4}']
    function GetItemNames(contextCoupon: Integer): OleVariant; safecall;
    procedure DeleteItems(participantCoupon: Integer; names: OleVariant; contextCoupon: Integer); safecall;
    procedure SetItemValues(participantCoupon: Integer; itemNames: OleVariant;
                            itemValues: OleVariant; contextCoupon: Integer); safecall;
    function GetItemValues(itemNames: OleVariant; onlyChanges: WordBool; contextCoupon: Integer): OleVariant; safecall;
  end;

// *********************************************************************//
// DispIntf:  IContextDataDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2AAE4991-A1FC-11D0-808F-00A0240943E4}
// *********************************************************************//
  IContextDataDisp = dispinterface
    ['{2AAE4991-A1FC-11D0-808F-00A0240943E4}']
    function GetItemNames(contextCoupon: Integer): OleVariant; dispid 201;
    procedure DeleteItems(participantCoupon: Integer; names: OleVariant; contextCoupon: Integer); dispid 202;
    procedure SetItemValues(participantCoupon: Integer; itemNames: OleVariant;
                            itemValues: OleVariant; contextCoupon: Integer); dispid 203;
    function GetItemValues(itemNames: OleVariant; onlyChanges: WordBool; contextCoupon: Integer): OleVariant; dispid 204;
  end;

// *********************************************************************//
// Interface: ISecureBinding
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F933331D-91C6-11D2-AB9F-4471FBC00000}
// *********************************************************************//
  ISecureBinding = interface(IDispatch)
    ['{F933331D-91C6-11D2-AB9F-4471FBC00000}']
    function InitializeBinding(bindeeCoupon: Integer; propertyNames: OleVariant;
                               propertyValues: OleVariant; var binderPublicKey: WideString): WideString; safecall;
    function FinalizeBinding(bindeeCoupon: Integer; const bindeePublicKey: WideString;
                             const mac: WideString): OleVariant; safecall;
  end;

// *********************************************************************//
// DispIntf:  ISecureBindingDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F933331D-91C6-11D2-AB9F-4471FBC00000}
// *********************************************************************//
  ISecureBindingDisp = dispinterface
    ['{F933331D-91C6-11D2-AB9F-4471FBC00000}']
    function InitializeBinding(bindeeCoupon: Integer; propertyNames: OleVariant;
                               propertyValues: OleVariant; var binderPublicKey: WideString): WideString; dispid 201;
    function FinalizeBinding(bindeeCoupon: Integer; const bindeePublicKey: WideString;
                             const mac: WideString): OleVariant; dispid 202;
  end;

// *********************************************************************//
// Interface: IContextParticipant
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {3E3DD272-998E-11D0-808D-00A0240943E4}
// *********************************************************************//
  IContextParticipant = interface(IDispatch)
    ['{3E3DD272-998E-11D0-808D-00A0240943E4}']
    function ContextChangesPending(contextCoupon: Integer; var reason: WideString): WideString; safecall;
    procedure ContextChangesAccepted(contextCoupon: Integer); safecall;
    procedure ContextChangesCanceled(contextCoupon: Integer); safecall;
    procedure CommonContextTerminated; safecall;
    procedure Ping; safecall;
  end;

// *********************************************************************//
// DispIntf:  IContextParticipantDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {3E3DD272-998E-11D0-808D-00A0240943E4}
// *********************************************************************//
  IContextParticipantDisp = dispinterface
    ['{3E3DD272-998E-11D0-808D-00A0240943E4}']
    function ContextChangesPending(contextCoupon: Integer; var reason: WideString): WideString; dispid 201;
    procedure ContextChangesAccepted(contextCoupon: Integer); dispid 202;
    procedure ContextChangesCanceled(contextCoupon: Integer); dispid 203;
    procedure CommonContextTerminated; dispid 204;
    procedure Ping; dispid 205;
  end;

// *********************************************************************//
// Interface: IContextAction
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {32A82A2D-76E9-4C4B-9716-74538B9A37A9}
// *********************************************************************//
  IContextAction = interface(IDispatch)
    ['{32A82A2D-76E9-4C4B-9716-74538B9A37A9}']
    function Perform(participantCoupon: Integer; itemNames: OleVariant; itemValues: OleVariant;
                     const appSignature: WideString; var actionCoupon: Integer;
                     var outItemNames: OleVariant; var outItemValues: OleVariant): WideString; safecall;
  end;

// *********************************************************************//
// DispIntf:  IContextActionDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {32A82A2D-76E9-4C4B-9716-74538B9A37A9}
// *********************************************************************//
  IContextActionDisp = dispinterface
    ['{32A82A2D-76E9-4C4B-9716-74538B9A37A9}']
    function Perform(participantCoupon: Integer; itemNames: OleVariant; itemValues: OleVariant;
                     const appSignature: WideString; var actionCoupon: Integer;
                     var outItemNames: OleVariant; var outItemValues: OleVariant): WideString; dispid 201;
  end;

// *********************************************************************//
// Interface: IContextFilter
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {637CD323-0175-45FA-AD5C-B7DE53CD1AFD}
// *********************************************************************//
  IContextFilter = interface(IDispatch)
    ['{637CD323-0175-45FA-AD5C-B7DE53CD1AFD}']
    procedure SetSubjectsOfInterest(participantCoupon: Integer; subjectNames: OleVariant); safecall;
    function GetSubjectsOfInterest(participantCoupon: Integer): OleVariant; safecall;
    procedure ClearFilter(participantCoupon: Integer); safecall;
  end;

// *********************************************************************//
// DispIntf:  IContextFilterDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {637CD323-0175-45FA-AD5C-B7DE53CD1AFD}
// *********************************************************************//
  IContextFilterDisp = dispinterface
    ['{637CD323-0175-45FA-AD5C-B7DE53CD1AFD}']
    procedure SetSubjectsOfInterest(participantCoupon: Integer; subjectNames: OleVariant); dispid 201;
    function GetSubjectsOfInterest(participantCoupon: Integer): OleVariant; dispid 202;
    procedure ClearFilter(participantCoupon: Integer); dispid 203;
  end;

// *********************************************************************//
// Interface: IContextSession
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {A76D5873-D2D3-4668-AE0F-37C8B6A380E4}
// *********************************************************************//
  IContextSession = interface(IDispatch)
    ['{A76D5873-D2D3-4668-AE0F-37C8B6A380E4}']
    function Create: IDispatch; safecall;
    procedure Activate(participantCoupon: Integer; const cmToActivate: IDispatch;
                       const nonce: WideString; const appSignature: WideString); safecall;
  end;

// *********************************************************************//
// DispIntf:  IContextSessionDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {A76D5873-D2D3-4668-AE0F-37C8B6A380E4}
// *********************************************************************//
  IContextSessionDisp = dispinterface
    ['{A76D5873-D2D3-4668-AE0F-37C8B6A380E4}']
    function Create: IDispatch; dispid 201;
    procedure Activate(participantCoupon: Integer; const cmToActivate: IDispatch;
                       const nonce: WideString; const appSignature: WideString); dispid 202;
  end;

// *********************************************************************//
// Interface: ISecureContextData
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {6F530680-BC14-11D1-90B1-76C60D000000}
// *********************************************************************//
  ISecureContextData = interface(IDispatch)
    ['{6F530680-BC14-11D1-90B1-76C60D000000}']
    function GetItemNames(contextCoupon: Integer): OleVariant; safecall;
    procedure SetItemValues(participantCoupon: Integer; itemNames: OleVariant;
                            itemValues: OleVariant; contextCoupon: Integer;
                            const appSignature: WideString); safecall;
    function GetItemValues(participantCoupon: Integer; names: OleVariant; onlyChanges: WordBool;
                           contextCoupon: Integer; const appSignature: WideString;
                           var managerSignature: WideString): OleVariant; safecall;
  end;

// *********************************************************************//
// DispIntf:  ISecureContextDataDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {6F530680-BC14-11D1-90B1-76C60D000000}
// *********************************************************************//
  ISecureContextDataDisp = dispinterface
    ['{6F530680-BC14-11D1-90B1-76C60D000000}']
    function GetItemNames(contextCoupon: Integer): OleVariant; dispid 201;
    procedure SetItemValues(participantCoupon: Integer; itemNames: OleVariant;
                            itemValues: OleVariant; contextCoupon: Integer;
                            const appSignature: WideString); dispid 202;
    function GetItemValues(participantCoupon: Integer; names: OleVariant; onlyChanges: WordBool;
                           contextCoupon: Integer; const appSignature: WideString;
                           var managerSignature: WideString): OleVariant; dispid 203;
  end;

// *********************************************************************//
// Interface: IImplementationInformation
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {41123600-6CE1-11D1-AB3F-E892F500000C}
// *********************************************************************//
  IImplementationInformation = interface(IDispatch)
    ['{41123600-6CE1-11D1-AB3F-E892F500000C}']
    function Get_ComponentName: WideString; safecall;
    function Get_RevMajorNum: WideString; safecall;
    function Get_RevMinorNum: WideString; safecall;
    function Get_PartNumber: WideString; safecall;
    function Get_Manufacturer: WideString; safecall;
    function Get_TargetOS: WideString; safecall;
    function Get_TargetOSRev: WideString; safecall;
    function Get_WhenInstalled: WideString; safecall;
    property ComponentName: WideString read Get_ComponentName;
    property RevMajorNum: WideString read Get_RevMajorNum;
    property RevMinorNum: WideString read Get_RevMinorNum;
    property PartNumber: WideString read Get_PartNumber;
    property Manufacturer: WideString read Get_Manufacturer;
    property TargetOS: WideString read Get_TargetOS;
    property TargetOSRev: WideString read Get_TargetOSRev;
    property WhenInstalled: WideString read Get_WhenInstalled;
  end;

// *********************************************************************//
// DispIntf:  IImplementationInformationDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {41123600-6CE1-11D1-AB3F-E892F500000C}
// *********************************************************************//
  IImplementationInformationDisp = dispinterface
    ['{41123600-6CE1-11D1-AB3F-E892F500000C}']
    property ComponentName: WideString readonly dispid 201;
    property RevMajorNum: WideString readonly dispid 202;
    property RevMinorNum: WideString readonly dispid 203;
    property PartNumber: WideString readonly dispid 204;
    property Manufacturer: WideString readonly dispid 205;
    property TargetOS: WideString readonly dispid 206;
    property TargetOSRev: WideString readonly dispid 207;
    property WhenInstalled: WideString readonly dispid 208;
  end;

// *********************************************************************//
// Interface: IAuthenticationRepository
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {12B28736-2895-11D2-BD6E-0060B0573ADC}
// *********************************************************************//
  IAuthenticationRepository = interface(IDispatch)
    ['{12B28736-2895-11D2-BD6E-0060B0573ADC}']
    function Connect(const applicationName: WideString): Integer; safecall;
    procedure Disconnect(bindingCoupon: Integer); safecall;
    procedure SetAuthenticationData(coupon: Integer; const logonName: WideString;
                                    const dataFormat: WideString; const userData: WideString;
                                    const appSignature: WideString); safecall;
    procedure DeleteAuthenticationData(coupon: Integer; const logonName: WideString;
                                       const dataFormat: WideString; const appSignature: WideString); safecall;
    function GetAuthenticationData(coupon: Integer; const logonName: WideString;
                                   const dataType: WideString; const appSignature: WideString;
                                   var userData: WideString): WideString; safecall;
  end;

// *********************************************************************//
// DispIntf:  IAuthenticationRepositoryDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {12B28736-2895-11D2-BD6E-0060B0573ADC}
// *********************************************************************//
  IAuthenticationRepositoryDisp = dispinterface
    ['{12B28736-2895-11D2-BD6E-0060B0573ADC}']
    function Connect(const applicationName: WideString): Integer; dispid 201;
    procedure Disconnect(bindingCoupon: Integer); dispid 202;
    procedure SetAuthenticationData(coupon: Integer; const logonName: WideString;
                                    const dataFormat: WideString; const userData: WideString;
                                    const appSignature: WideString); dispid 203;
    procedure DeleteAuthenticationData(coupon: Integer; const logonName: WideString;
                                       const dataFormat: WideString; const appSignature: WideString); dispid 204;
    function GetAuthenticationData(coupon: Integer; const logonName: WideString;
                                   const dataType: WideString; const appSignature: WideString;
                                   var userData: WideString): WideString; dispid 205;
  end;

// *********************************************************************//
// The Class CoContextManager provides a Create and CreateRemote method to
// create instances of the default interface IContextManager exposed by
// the CoClass ContextManager. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoContextManager = class
    class function Create: IContextManager;
    class function CreateRemote(const MachineName: string): IContextManager;
  end;

implementation

uses System.Win.ComObj;

class function CoContextManager.Create: IContextManager;
begin
  Result := CreateComObject(CLASS_ContextManager) as IContextManager;
end;

class function CoContextManager.CreateRemote(const MachineName: string): IContextManager;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ContextManager) as IContextManager;
end;

end.

