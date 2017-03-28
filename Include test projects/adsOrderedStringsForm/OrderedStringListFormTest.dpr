program OrderedStringListFormTest;

uses
  Forms,
  adsOrderedStringsFrm,
   OrderedStringListFormTestFrm in 'OrderedStringListFormTestFrm.pas' {OrderedStringListFormTestForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TOrderedStringListFormTestForm, OrderedStringListFormTestForm);
  Application.CreateForm(TadsOrderedStringsForm, adsOrderedStringsForm);
  Application.Run;
end.
