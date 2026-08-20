program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	NN = 500 * 1000;
var
	n, i, d: int32;
	mn, dh, ans: int64;
	h: array [0 .. NN] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	h[0] := 0;
	for i := 1 to n do begin
		read(d);
		h[i] := max(h[i-1] + d, 0);
	end;
	readln;

	mn := high(int64);
	ans := h[n] div 2;
	for i := n downto 1 do begin
		mn := min(mn, h[i]);
		dh := h[i] - h[i] div 2;
		dec(dh, max(dh - mn, 0));
		ans := min(ans, h[n] - dh);
	end;

	writeln(ans);
end.
