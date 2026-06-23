program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, m, i, l, r: int8;
	t, u: string;
	ans: boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(t);
	readln(u);
	n := length(t);
	m := length(u);

	ans := false;
	for r := m to n do
		if not ans then begin
			l := r-m+1;
			ans := true;
			for i := l to r do
				if ans then
					ans := (t[i] = '?') or (t[i] = u[i-l+1]);
		end;

	if ans then
		writeln('Yes')
	else
		writeln('No');
end.
