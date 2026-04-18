# Задатак: C_AtCoder_AAC_Contest.pas

```pascal
program C_AtCoder_AAC_Contest;
uses
	math;
var
	ntc, tci: int32;
	na, nb, nc, ans: int64;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(na, nb, nc);
		ans := min(na, nc);
		ans := min(ans, (na+nb+nc) div 3);
		writeln(ans);

	end;
end.

```
