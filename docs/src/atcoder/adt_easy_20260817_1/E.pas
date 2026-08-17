program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	t, ans: int64;
	ch: char;
	s: string;
	c: array ['a' .. 'z'] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);

	for ch := 'a' to 'z' do c[ch] := 0;

	for ch in s do inc(c[ch]);

	ch := 'a';
	while (ch <= 'z') and (c[ch] < 2) do inc(ch);

	ans := 0;
	if ch <= 'z' then inc(ans);

	t := 0;
	for ch := 'a' to 'z' do begin
		inc(ans, int64(t) * c[ch]);
		inc(t, c[ch]);
	end;

	writeln(ans);
end.
