program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	h, w, i, ans: int8;
	s: string;
	ch: char;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(h, w);

	ans := 0;
	for i := 1 to h do begin
		readln(s);
		for ch in s do
			if ch = '#' then inc(ans);
	end;

	writeln(ans);
end.
