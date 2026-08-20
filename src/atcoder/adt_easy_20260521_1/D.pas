program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #adjacency #matrix #list
var
	n, i, j, a: int8;
	space: boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for i := 1 to n do begin
		space := false;
		for j := 1 to n do begin
			read(a);
			if a = 1 then begin
				if space then write(' ');
				space := true;
				write(j);
			end;
		end;
		readln;
		writeln;
	end;
end.
