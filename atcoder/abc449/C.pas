program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 500 * 1000;
var
	n, i, j, l, r: int32;
	ans: int64;
	ch: char;
	s: string;
	c: array ['a' .. 'z'] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, l, r);
	readln(s);

	ans := 0;
	for ch := 'a' to 'z' do c[ch] := 0;

	for j := l+1 to n do begin
		i := j-r-1;
		if i >= 1 then dec(c[s[i]]);
		inc(c[s[j-l]]);
		inc(ans, c[s[j]]);
	end;

	writeln(ans);
end.
