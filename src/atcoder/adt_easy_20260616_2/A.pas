program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, j, a, b: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for i := 1 to n do begin
		b := 0;
		for j := 1 to 7 do begin
			read(a);
			inc(b, a);
		end;
		write(b);
		if i < n then write(' ');
	end;
	readln;
	writeln;
end.
