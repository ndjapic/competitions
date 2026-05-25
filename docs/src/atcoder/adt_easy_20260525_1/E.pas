program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #count #query
const
	NN = 200 * 1000;
var
	n, q, i, x, abc: int32;
	c: char;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	randomize;

	readln(n, q);

	readln(s);

	abc := 0;
	for i := 3 to n do
		if copy(s, i-2, 3) = 'ABC' then inc(abc);

	for i := 1 to q do begin
		readln(x, c, c);
		if c <> s[x] then begin

			if (x-2 > 0) and (copy(s, x-2, 3) = 'ABC') then
				dec(abc)
			else if (x-1 > 0) and (x+1 <= n) and (copy(s, x-1, 3) = 'ABC') then
				dec(abc)
			else if (x+2 <= n) and (copy(s, x, 3) = 'ABC') then
				dec(abc);

			s[x] := c;

			if (x-2 > 0) and (copy(s, x-2, 3) = 'ABC') then
				inc(abc)
			else if (x-1 > 0) and (x+1 <= n) and (copy(s, x-1, 3) = 'ABC') then
				inc(abc)
			else if (x+2 <= n) and (copy(s, x, 3) = 'ABC') then
				inc(abc);

		end;
		writeln(abc);
	end;
end.
