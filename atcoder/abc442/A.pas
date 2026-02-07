program A_;
var
	n, i, ans: int32;
	s: string;

begin
	readln(s);
	n := length(s);
	ans := 0;
	for i := 1 to n do
		if (s[i] = 'i') or (s[i] = 'j') then
			inc(ans);
	writeln(ans);
end.
