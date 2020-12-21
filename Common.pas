unit Common;

interface

uses CCOW_TLB, Classes;

type

  PParticipant = ^TParticipant;

  TParticipant = record
    contextParticipant: IContextParticipant;
    participantCoupon: Integer;
    title: WideString;
    survey: WordBool;
    wait: WordBool;
    suspended: WordBool;
  end;

  PContext = ^TContext;

  TContext = record
    participantCoupon: Integer;
    contextCoupon: Integer;
    contextItems: TStrings;
  end;

implementation

end.
