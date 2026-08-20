program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #custom #sort #prefix #counter
uses
	generics.collections,
	generics.defaults, math;
const
	NN = 200 * 1000;
type
	tperson = record
		w: int32;
		s: int8;
	end;
var
	n, i, ans: int32;
	s: string;
	a: tlist<tperson>;
	p: tperson;
	c1: array [0 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function personcompare(constref l, r: tperson): int32;
begin
	result := comparevalue(l.w, r.w);
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	randomize;

	readln(n);
	readln(s);

	a := tlist<tperson>.create;
	for i := 1 to n do begin
		read(p.w);
		p.s := ord(s[i]) - ord('0');
		a.add(p);
		a.exchange(i-1, random(i));
	end;
	readln;
	a.sort(tcomparer<tperson>.construct(personcompare));

	c1[0] := 0;
	for i := 0 to n-1 do
		c1[i+1] := c1[i] + a[i].s;

	ans := max(c1[n], n - c1[n]);
	for i := 1 to n-1 do
		if a[i-1].w < a[i].w then
			ans := max(ans, i - c1[i] + c1[n] - c1[i]);

	writeln(ans);
	a.free;
end.
