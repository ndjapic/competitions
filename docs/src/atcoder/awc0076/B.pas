program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #sort #custom
uses
	Generics.Defaults, Generics.Collections, Math;
type
	TParticipant = record
		a, b, i: int32;
	end;

function CompareParticipants(constref Left, Right: TParticipant): Integer;
begin
	Result := - CompareValue(Left.b, Right.b);
	if Result = 0 then
		Result := - CompareValue(Left.a, Right.a);
	if Result = 0 then
		Result := CompareValue(Left.i, Right.i);
end;

var
	n, i: int32;
	p: TParticipant;
	participants: TList<TParticipant>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	Randomize;

	readln(n);

	participants := TList<TParticipant>.Create;
	participants.Capacity := n;

	for i := 0 to n-1 do begin
		ReadLn(p.a, p.b);
		p.i := i+1;
		participants.Add(p);
		participants.Exchange(i, Random(i+1));
	end;

	participants.Sort(TComparer<TParticipant>.Construct(CompareParticipants));

	for i := 0 to n - 1 do
		Writeln(participants[i].i);

	participants.Free;
end.
