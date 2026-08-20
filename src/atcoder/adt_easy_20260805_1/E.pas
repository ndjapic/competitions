program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #custom #sort
uses
	Generics.Collections,
	Generics.Defaults, Math;
const
	N = 5;
	M = 31;
var
	e, i, k: int8;
	j: uint8;
	a: array [1 .. N] of int32;
	name: array [1 .. M] of string;
	score: array [1 .. M] of int32;
	p: TList<int8>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function cmp(constref l, r: int8): int32;
begin
	Result := CompareValue(score[r], score[l]);
	if Result = 0 then begin
		if name[l] < name[r] then
			Result := -1
		else if name[l] > name[r] then
			Result := 1
		else
			Result := 0;
	end;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	Randomize;

	for e := 1 to N do read(a[e]);
	readln;

	p := TList<int8>.Create;
	for j := 31 downto 1 do begin
		setlength(name[j], PopCnt(j));
		score[j] := 0;
		k := p.Add(j);
		p.Exchange(k, Random(k+1));

		i := 0;
		for e := N-1 downto 0 do
			if odd(j shr e) then begin
				inc(i);
				name[j][i] := chr(ord('A') + N-1-e);
				inc(score[j], a[N-e]);
			end;
	end;
	p.Sort(TComparer<int8>.Construct(cmp));

	for j in p do writeln(name[j]);

	p.Free;
end.
