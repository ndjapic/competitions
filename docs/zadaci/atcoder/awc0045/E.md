# Задатак: E.pas

```pascal
program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
var
	N, W, K, i, l, r: Int32;
	s, EvaluationScore, ans: Int64;
	A, dq: array of Int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	ReadLn(N, W, K);

	SetLength(A, N);
	SetLength(dq, N);

	l := 0;
	r := 0;
	s := 0;
	ans := Low(Int64);

	for i := 0 to N-1 do begin
		Read(A[i]);

		while (r > l) and (A[dq[r - 1]] >= A[i]) do Dec(r);

		dq[r] := i;
		Inc(r);
		if dq[l] <= i - W then Inc(l);

		s := s + A[i];
		if i >= W then s := s - A[i - W];

		if i >= W - 1 then begin
			EvaluationScore := s + Int64(K) * A[dq[l]];
			ans := max(ans, EvaluationScore);
		end;
	end;
	ReadLn;

	writeln(ans);
end.

```
