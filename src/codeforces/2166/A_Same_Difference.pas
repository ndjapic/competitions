program A_Same_Difference;
{$MODE DELPHI}
var
	notc, tci, n, i, ans: int32;
	s: string;

begin
	readln(notc);
	for tci := 1 to notc do begin
		readln(n);
		readln(s);

		ans := 0;
		for i := n-1 downto 1 do
			if s[i] <> s[n] then inc(ans);

		writeln(ans);
	end;
end.
