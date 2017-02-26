unit uCreature;

interface

function GetMaxCreatureLife(ACreature, ABoss: Byte): Word;

implementation

function GetMaxCreatureLife(ACreature, ABoss: Byte): Word;
begin
  if (ABoss = 4) then Result := 100{0} else
    Result := ((ACreature + 3) * 3) * (ABoss + 1);
end;

end.
