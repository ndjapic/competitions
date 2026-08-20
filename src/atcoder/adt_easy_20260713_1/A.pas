program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, a: int8;
	space: boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	space := false;
	for i := 1 to n do begin
		read(a);
		if not odd(a) then begin
			if space then
				write(' ')
			else
				space := true;
			write(a);
		end;
	end;
	readln;
	writeln;
end.
