program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #hashtable #bisect
uses
	Generics.Collections, SysUtils;
type
	TIndices = TList<int32>;
var
	n, q, i, j, l, r: int32;
	Line, s, t: string;
	Tokens: TStringArray;
	ind: TDictionary<string, TIndices>;
	a: TIndices;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function BisectR(a: TIndices; x: int32): int32;
var
	l, r, m: int32;
begin
	l := -1;
	r := a.Count;
	while r-l > 1 do begin
		m := (r+l) div 2;
		if x < a[m] then // a[l] <= x < a[r]
			r := m
		else
			l := m;
	end;
	result := r;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, q);

	Readln(Line);
	Tokens := Line.Split([' '], TStringSplitOptions.ExcludeEmpty);

	ind := TDictionary<string, TIndices>.Create;
	for i := 1 to n do begin
		s := Tokens[i-1];
		if not ind.TryGetValue(s, a) then begin
			a := TIndices.Create;
			ind.AddOrSetValue(s, a);
		end;
		a.Add(i);
	end;

	for j := 1 to q do begin
		Readln(Line);
		Tokens := Line.Split([' '], TStringSplitOptions.ExcludeEmpty);

		l := StrToInt(Tokens[0]);
		r := StrToInt(Tokens[1]);
		t := Tokens[2];

		if ind.TryGetValue(t, a) then
			writeln(BisectR(a, r) - BisectR(a, l-1))
		else
			writeln(0);
	end;

	for a in ind.Values do a.Free;
	ind.Free;
end.
