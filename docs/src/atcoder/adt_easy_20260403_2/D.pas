program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 200 * 1000;
var
	n, i, k: int32;
	s, t: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function find_k(i: int32): int32;
begin
	Result := ord(t[i]) - ord(s[i]);
	if Result < 0 then inc(Result, 26);
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);
	readln(t);
	n := length(s);

	k := find_k(1);

	i := 2;
	while (i <= n) and (find_k(i) = k) do inc(i);

	if i > n then
		writeln('Yes')
	else
		writeln('No');
end.
