program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #custom #sort
uses
	Generics.Collections,
	Generics.Defaults, Math;
const
	NN = 300 * 1000;
var
	n, k, i, o: int32;
	a, b: array [1 .. NN] of int32;
	regular: array [1 .. NN] of boolean;
	p: TList<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function PlayerCompare(constref l, r: int32): int32;
begin
	Result := CompareValue(a[r] + b[r], a[l] + b[l]);
	if Result = 0 then
		Result := CompareValue(l, r);
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	Randomize;

	readln(n, k);

	p := TList<int32>.Create;
	for i := 1 to n do begin
		readln(a[i], b[i]);
		p.Add(i);
		p.Exchange(i-1, Random(i));
	end;
	p.Sort(TComparer<int32>.Construct(PlayerCompare));

	for i := 1 to n do regular[i] := false;

	for o := 0 to k-1 do begin
		i := p[o];
		regular[i] := true;
	end;

	for i := 1 to n do
		if regular[i] then writeln(i);

	p.Free;
end.
