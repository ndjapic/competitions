program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 200 * 1000;
var
	n, q, i, a, ans: int32;
	c: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, q);

	setlength(c, n);
	for a := 1 to n do c[a] := 'w';

	ans := 0;
	for i := 1 to q do begin
		read(a);
		case c[a] of

			'w': begin
				if n = 1 then
					inc(ans)
				else if a = 1 then begin
					if (c[a+1] = 'w') then inc(ans);
				end else if a = n then begin
					if (c[a-1] = 'w') then inc(ans);
				end else if (c[a-1] = 'w') and (c[a+1] = 'w') then
					inc(ans)
				else if (c[a-1] = 'b') and (c[a+1] = 'b') then
					dec(ans);
				c[a] := 'b';
			end;

			'b': begin
				if n = 1 then
					dec(ans)
				else if a = 1 then begin
					if (c[a+1] = 'w') then dec(ans);
				end else if a = n then begin
					if (c[a-1] = 'w') then dec(ans);
				end else if (c[a-1] = 'w') and (c[a+1] = 'w') then
					dec(ans)
				else if (c[a-1] = 'b') and (c[a+1] = 'b') then
					inc(ans);
				c[a] := 'w';
			end;

		end;
		writeln(ans);
	end;
	readln;
end.
