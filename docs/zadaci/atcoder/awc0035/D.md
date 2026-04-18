# Задатак: D.pas

```pascal
program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Defaults, Generics.Collections, Math;
type
	TJob = record
		d, t: int32;
	end;
var
	n, m, i, j, x, ans: int32;
	job: TJob;
	v: TList<int32>;
	jobs: TList<TJob>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function CompareJobs(constref Left, Right: TJob): Integer;
begin
	Result := Sign(int64(Left.d) * Right.t - int64(Right.d) * Left.t);
end;

begin
	randomize;
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	v := TList<int32>.Create;
	for i := 0 to n-1 do begin
		read(x);
		v.Add(x);
		v.Exchange(i, random(i+1));
	end;
	readln;
	v.Sort;

	jobs := TList<TJob>.Create;
	for j := 0 to m-1 do begin
		readln(job.d, job.t);
		jobs.Add(job);
		jobs.Exchange(j, random(j+1));
	end;
	jobs.Sort(TComparer<TJob>.Construct(CompareJobs));

	i := n-1;
	j := m-1;
	ans := 0;
	while (i >= 0) and (j >= 0) do begin
		if int64(v[i]) * jobs[j].t >= jobs[j].d then begin
			inc(ans);
			dec(i);
		end;
		dec(j);
	end;

	writeln(ans);

	v.Free;
	jobs.Free;
end.

```
