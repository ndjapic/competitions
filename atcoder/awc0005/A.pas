program _A;
var
	n, k, i, p: int32;
	ans: int64;

begin
	readln(n, k);

	ans := 0;
	for i := 1 to n do begin
		read(p);
		if p mod k = 0 then inc(ans, p);
	end;
	readln;

	writeln(ans);
end.
