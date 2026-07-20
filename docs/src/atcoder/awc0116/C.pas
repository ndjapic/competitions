program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	NN = 1000 * 1000;
var
	n, k, i, ans: int32;
	h, l, r: array [1 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);

	for i := 1 to n do begin
		read(h[i]);
		l[i] := i;
		r[i] := i;
	end;
	readln;

	for i := 2 to n do
		if h[i-1] < h[i] then
			l[i] := l[i-1];

	for i := n-1 downto 1 do
		if h[i] > h[i+1] then
			r[i] := r[i+1];

	ans := 0;
	for i := 1 to n do
		if max(h[i] - h[l[i]], h[i] - h[r[i]]) >= k then
			ans := max(ans, r[i] - l[i] + 1);

	writeln(ans);
end.
