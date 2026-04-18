program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 30;
var
	n, i, j: int8;
	a: array [0 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for i := 0 to n-1 do begin
		a[i] := 1;
		for j := i-1 downto 1 do inc(a[j], a[j-1]);
		for j := 0 to i do begin
			write(a[j]);
			if j < i then write(' ');
		end;
		writeln;
	end;
end.
