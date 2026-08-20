program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Collections, Generics.Defaults, Math;
const
	nn = 300 * 1000;
type
	TMedicine = record
		a, b: int32;
	end;
var
	n, k, i: int32;
	s: int64;
	meds: TList<TMedicine>;
	med: TMedicine;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function CompareMeds(constref Left, Right: TMedicine): int32;
begin
	Result := CompareValue(Left.a, Right.a);
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	randomize;

	readln(n, k);

	meds := TList<TMedicine>.Create;
	s := 0;
	for i := 0 to n-1 do begin
		readln(med.a, med.b);
		inc(s, med.b);
		meds.Add(med);
		meds.Exchange(i, random(i+1));
	end;
	meds.Sort(TComparer<TMedicine>.Construct(CompareMeds));

	i := -1;
	while s > k do begin
		inc(i);
		dec(s, meds[i].b);
	end;

	if i = -1 then
		writeln(1)
	else
		writeln(meds[i].a + 1);

	meds.Free;
end.
