unit Actor.Remote.Encoding;

interface

type
  TCommandType = (ctOpen, ctClose, ctRegister, ctUnregister, ctActorOf, ctSendTo);

function EncodeOpen(const PeerIP: String; PeerPort: Cardinal): String;
procedure DecodeOpen(const S: String; out PeerIP: String; out PeerPort: Cardinal);
function EncodeClose(const PeerIP: String; PeerPort: Cardinal): String;
procedure DecodeClose(const S: String; out PeerIP: String; out PeerPort: Cardinal);
function EncodeRegister(const PeerIP: String; PeerPort: Cardinal; const ActorClass: String): String;
procedure DecodeRegister(const S: String; out PeerIP: String; out PeerPort: Cardinal; out ActorClass: String);
function EncodeUnregister(const PeerIP: String; PeerPort: Cardinal; const ActorClass: String): String;
procedure DecodeUnregister(const S: String; out PeerIP: String; out PeerPort: Cardinal; out ActorClass: String);
function EncodeActorOf(const PeerIP: String; PeerPort: Cardinal; const ActorClass, Parent: String): String;
procedure DecodeActorOf(const S: String; out PeerIP: String; out PeerPort: Cardinal; out ActorClass, Parent: String);
function EncodeSendTo(const Sender, Receiver: String; const M: String): String;
procedure DecodeSendTo(const S: String; out Sender, Receiver: String; out M: String);
function CommandTypeOf(const S: String): TCommandType;

implementation

uses
  SysUtils;

function EncodeOpen(const PeerIP: String; PeerPort: Cardinal): String;
begin
  Result := Format('O|%s:%d', [PeerIP, PeerPort]);
end;

procedure DecodeOpen(const S: String; out PeerIP: String; out PeerPort: Cardinal);
begin
  PeerIP := Copy(S, 3, Pos(':', S)-3);
  PeerPort := StrToInt(Copy(S, Pos(':', S)+1, MAXINT));
end;

function EncodeClose(const PeerIP: String; PeerPort: Cardinal): String;
begin
  Result := Format('C|%s:%d', [PeerIP, PeerPort]);
end;

procedure DecodeClose(const S: String; out PeerIP: String; out PeerPort: Cardinal);
begin
  DecodeOpen(S, PeerIP, PeerPort);
end;

function EncodeRegister(const PeerIP: String; PeerPort: Cardinal; const ActorClass: String): String;
begin
  Result := Format('R|%s:%d|%s', [PeerIP, PeerPort, ActorClass]);
end;

procedure DecodeRegister(const S: String; out PeerIP: String; out PeerPort: Cardinal; out ActorClass: String);
var
  Text: String;
begin
  Text := Copy(S, 3, MAXINT);
  PeerIP := Copy(Text, 1, Pos(':', Text)-1);
  Text := Copy(Text, Pos(':', Text)+1, MAXINT);
  PeerPort := StrToInt(Copy(Text, 1, Pos('|', Text)-1));
  ActorClass := Copy(Text, Pos('|', Text)+1, MAXINT);
end;

function EncodeUnregister(const PeerIP: String; PeerPort: Cardinal; const ActorClass: String): String;
begin
  Result := Format('U|%s:%d|%s', [PeerIP, PeerPort, ActorClass]);
end;

procedure DecodeUnregister(const S: String; out PeerIP: String; out PeerPort: Cardinal; out ActorClass: String);
begin
  DecodeRegister(S, PeerIP, PeerPort, ActorClass);
end;

function EncodeActorOf(const PeerIP: String; PeerPort: Cardinal; const ActorClass, Parent: String): String;
begin
  Result := Format('A|%s:%d|%s|%s', [PeerIP, PeerPort, ActorClass, Parent]);
end;

procedure DecodeActorOf(const S: String; out PeerIP: String; out PeerPort: Cardinal; out ActorClass, Parent: String);
var
  Text: String;
begin
  Text := Copy(S, 3, MAXINT);
  PeerIP := Copy(Text, 1, Pos(':', Text)-1);
  Text := Copy(Text, Pos(':', Text)+1, MAXINT);
  PeerPort := StrToInt(Copy(Text, 1, Pos('|', Text)-1));
  Text := Copy(Text, Pos('|', Text)+1, MAXINT);
  ActorClass := Copy(Text, 1, Pos('|', Text)-1);
  Parent := Copy(Text, Pos('|', Text)+1, MAXINT);
end;

function EncodeSendTo(const Sender, Receiver: String; const M: String): String;
begin
  Result := Format('S|%s|%s|%s', [Sender, Receiver, M]);
end;

procedure DecodeSendTo(const S: String; out Sender, Receiver: String; out M: String);
var
  Text: String;
begin
  Text := Copy(S, 3, MAXINT);
  Sender := Copy(Text, 1, Pos('|', Text)-1);
  Text := Copy(Text, Pos('|', Text)+1, MAXINT);
  Receiver := Copy(Text, 1, Pos('|', Text)-1);
  M := Copy(Text, Pos('|', Text)+1, MAXINT);
end;

function CommandTypeOf(const S: String): TCommandType;
begin
  if S[1] = 'O' then Result := ctOpen
  else if S[1] = 'C' then Result := ctClose
  else if S[1] = 'R' then Result := ctRegister
  else if S[1] = 'U' then Result := ctUnregister
  else if S[1] = 'A' then Result := ctActorOf
  else Result := ctSendTo
end;

end.
