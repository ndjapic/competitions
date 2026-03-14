program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
    n, i, j, a: int8;

begin
    readln(n);

	for i := 1 to n do begin
		for j := 1 to n do begin
			read(a);
			if a = 1 then write(j, ' ');
		end;
		readln;
		writeln;
	end;
end.
