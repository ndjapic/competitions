program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #default #sort #reverse #time #bisect
uses
	generics.collections,
	generics.defaults, math;
const
	NN = 300 * 1000;
var
	n, q, i, j, t: int32;
	h, l, mx: array [1 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function bisect(t: int32): int32;
var
	bl, br, bm: int32;
begin
	bl := 0;
	br := n;
	while br - bl > 1 do begin
		bm := (bl + br) div 2;
		if t < l[bm] then
			br := bm
		else
			bl := bm;
	end;
	result := br;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	randomize;

	readln(n);
	for i := 1 to n do readln(h[i], l[i]);

	mx[n] := h[n];
	for i := n-1 downto 1 do mx[i] := max(mx[i+1], h[i]);

	readln(q);
	for j := 1 to q do begin
		read(t);
		writeln(mx[bisect(t)]);
	end;
	readln;
end.
