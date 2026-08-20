program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	s: string;
	ch: char;
	o: int8;
	up, lo, ans: boolean;
	useen, lseen: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);

	up := false;
	lo := false;
	useen := 0;
	lseen := 0;
	ans := true;

	for ch in s do
		if ans then begin
			if ch < 'a' then begin

				up := true;
				o := ord(ch) - ord('A');

				if odd(useen shr o) then
					ans := false
				else
					inc(useen, 1 shl o);

			end else begin

				lo := true;
				o := ord(ch) - ord('A');

				if odd(lseen shr o) then
					ans := false
				else
					inc(lseen, 1 shl o);

			end;
		end;

	if up and lo and ans then
		writeln('Yes')
	else
		writeln('No');
end.
