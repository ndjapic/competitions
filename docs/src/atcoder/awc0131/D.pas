program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 200 * 1000;
var
	n, q, m, i, j: int32;
	v: int64;
	p: array [1 .. NN] of int64;
	pre, suf: array [1 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, q, m);

	pre[1] := 1;
	for i := 1 to n-1 do begin
		read(p[i]);
		pre[i+1] := p[i] * pre[i] mod m;
	end;
	readln(p[n]);

	suf[n] := 1;
	for i := n downto 2 do
		suf[i-1] := p[i] * suf[i] mod m;

	for j := 1 to q do begin
		readln(i, v);
		v := v * pre[i] mod m;
		v := v * suf[i] mod m;
		writeln(v);
	end;
end.
