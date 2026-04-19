# Problem: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	notc, tci, a, b, c: int32;
	m, bc, ca, ab, abc, ans, bns, cns, bcns, cans, abns, abcns, wa, wb, wc: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function gcd(a, b: int64): int64;
begin
    if b = 0 then
        gcd := a
    else
        gcd := gcd(b, a mod b);
end;

function lcm(a, b: int64): int64;
begin
    lcm := int64(a) div gcd(a, b) * b;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	readln(notc);
	for tci := 1 to notc do begin

		readln(a, b, c, m);

		bc := lcm(b, c);
		ca := lcm(c, a);
		ab := lcm(a, b);

		abc := lcm(ab, c);
		abcns := m div abc;

		bcns := m div bc - abcns;
		cans := m div ca - abcns;
		abns := m div ab - abcns;

		ans := m div a - abcns - cans - abns;
		bns := m div b - abcns - abns - bcns;
		cns := m div c - abcns - bcns - cans;

		wa := ans * 6 + cans * 3 + abns * 3 + abcns * 2;
		wb := bns * 6 + abns * 3 + bcns * 3 + abcns * 2;
		wc := cns * 6 + bcns * 3 + cans * 3 + abcns * 2;

		writeln(wa, ' ', wb, ' ', wc);

	end;
end.

```
