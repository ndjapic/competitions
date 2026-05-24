program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	HH = 20;
var
	h, w, i, j, black: int8;
	ans: boolean;
	s: array [1 .. HH] of string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(h, w);

	for i := 1 to h do readln(s[i]);

	ans := true;
	for i := 1 to h do
		for j := 1 to w do
			if ans and (s[i][j] = '#') then begin
				black := 0;
				if (i > 1) and (s[i-1][j] = '#') then inc(black);
				if (j > 1) and (s[i][j-1] = '#') then inc(black);
				if (i < h) and (s[i+1][j] = '#') then inc(black);
				if (j < w) and (s[i][j+1] = '#') then inc(black);
				if (black <> 2) and (black <> 4) then ans := false;
			end;

	if ans then
		writeln('Yes')
	else
		writeln('No');
end.
