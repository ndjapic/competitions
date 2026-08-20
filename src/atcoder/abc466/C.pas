program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, j, x: int32;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	x := 0;
	i := 1;
	j := 2;

	while j <= n do begin
		if i < j then begin
			writeln('? ', i, ' ', j); flush(output);
			readln(s);

			case s[1] of
				'Y': begin
					inc(x, j-i);
					inc(j);
				end;
				'N':  inc(i);
			end;
		end else
			inc(j);
	end;

	writeln('! ', x); flush(output);
end.
