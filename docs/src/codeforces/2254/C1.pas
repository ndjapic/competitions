program _C1;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	notc, tci, n, i, o0, o1, e0, e1: int32;
	a, b: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);
		readln(a);
		readln(b);

		o0 := 0;
		o1 := 0;
		e0 := 0;
		e1 := 0;

		for i := 1 to n do
			if odd(i) then begin

				case a[i] of
					'0': dec(o0);
					'1': dec(o1);
				end;

				case b[i] of
					'0': inc(o0);
					'1': inc(o1);
				end;

			end else begin

				case a[i] of
					'0': dec(e0);
					'1': dec(e1);
				end;

				case b[i] of
					'0': inc(e0);
					'1': inc(e1);
				end;

			end;

		if (o0 = 0) and (o1 = 0) and (e0 = 0) and (e1 = 0) then
			writeln('YES')
		else
			writeln('NO');

	end;
end.
