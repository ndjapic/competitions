program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, j, ans: int32;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);
	n := length(s);

	ans := 0;
	j := 1;
	for i := 1 to n do
		case j mod 2 of

			0: case s[i] of
				'i': begin
					inc(ans);
					inc(j, 2);
				end;
				'o': inc(j);
			end;

			1: case s[i] of
				'i': inc(j);
				'o': begin
					inc(ans);
					inc(j, 2);
				end;
			end;

		end;

	if s[n] = 'i' then inc(ans);
	writeln(ans);
end.
