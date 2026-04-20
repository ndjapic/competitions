program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	a, b, c, i: int8;
	ans: int32;
	d: array [1 .. 9] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(a, b, c);

	for i := 1 to 9 do d[i] := 0;
	inc(d[a]);
	inc(d[b]);
	inc(d[c]);

	ans := 0;
	for i := 9 downto 1 do
		while d[i] > 0 do begin
			ans := ans * 10 + i;
			dec(d[i]);
		end;

	writeln(ans);
end.
