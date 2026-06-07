program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, x: int8;
	ch: char;
	s: string;
	ans: boolean;
	c: array ['a' .. 'z'] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);
	n := length(s);

	for ch := 'a' to 'z' do c[ch] := 0;

	for i := 1 to n do inc(c[s[i]]);

	ans := true;
	i := 1;
	while (i <= n) and ans do begin
		x := 0;
		for ch := 'a' to 'z' do
			if c[ch] = i then inc(x);
		ans := (x = 0) or (x = 2);
		inc(i);
	end;

	if ans then
		writeln('Yes')
	else
		writeln('No');
end.
