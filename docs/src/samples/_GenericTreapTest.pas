program GenericTreapTest;
{$mode delphi}
uses 
	UGenericTreap, Generics.Defaults, Math;

function TreapCompare(constref Left, Right: Integer): Integer;
begin
	Result := CompareValue(Left, Right);
end;

var
	t: TTreap<Integer>;
begin
	t := TTreap<Integer>.Create(TComparer<Integer>.Construct(TreapCompare));
	try
		t.Add(10);
		t.Add(20);
		t.Add(5);
		t.Add(10); // Duplikat
		
		WriteLn('Min: ', t.GetMin); // 5
		WriteLn('Max: ', t.GetMax); // 20
		WriteLn('Razlika: ', t.GetMax - t.GetMin);

		t.Remove(5);
		WriteLn('Novi Min: ', t.GetMin); // 10
	finally
		t.Free;
	end;
end.
