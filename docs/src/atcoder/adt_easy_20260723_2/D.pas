program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	NN = 100;
	INF = NN * NN;
type
	TGrid = array of string;
var
	n, i, e: int8;
	ans: int32;
	s, t: TGrid;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function rotated(g: TGrid): TGrid;
var
	i, j: int8;
begin
	setlength(result, n+1);
	for i := 1 to n do begin
		setlength(result[i], n);
		for j := 1 to n do
			result[i][j] := g[n+1-j][i];
	end;
end;

function differences(s, t: TGrid): int32;
var
	i, j: int8;
begin
	result := 0;
	for i := 1 to n do
		for j := 1 to n do
			if s[i][j] <> t[i][j] then inc(result);
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	setlength(s, n+1);
	setlength(t, n+1);

	for i := 1 to n do readln(s[i]);
	for i := 1 to n do readln(t[i]);

	ans := INF;
	for e := 0 to 3 do begin
		ans := min(ans, differences(s, t) + e);
		s := rotated(s);
	end;

	writeln(ans);
end.
