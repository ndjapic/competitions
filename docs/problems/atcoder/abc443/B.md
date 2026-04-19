# Problem: B.pas

```pascal
program _B;
var
	n, k, ans, beans: int32;

begin
	readln(n, k);
	beans := n;
	ans := n;
	while beans < k do begin
		inc(ans);
		inc(beans, ans);
	end;
	writeln(ans - n);
end.

```
