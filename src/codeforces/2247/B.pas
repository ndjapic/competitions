program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	notc, tci, n, i, k, m: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(notc);
	for tci := 1 to notc do begin

		readln(n, k, m);

		if k <= m then begin
			writeln('YES');
			for i := 1 to n do begin
				if i mod k > 0 then
					write('1')
				else
					write(m-k+1);
				if i < n then write(' ');
			end;
			writeln;
		end else
			writeln('NO');

	end;
end.
