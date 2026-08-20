program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, k, i, a: int8;
	space: boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);

	space := false;
	for i := 1 to n do begin
		read(a);

		if a mod k = 0 then begin
			if space then
				write(' ')
			else
				space := true;

			write(a div k);
		end;
	end;
	readln;
	writeln;
end.
