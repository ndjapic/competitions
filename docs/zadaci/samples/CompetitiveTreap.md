# Задатак: CompetitiveTreap.pas

```pascal
program CompetitiveTreap;
{$MODE DELPHI}
uses SysUtils;
const MAXN = 200005;
var
	val, prio, sz: array[0..MAXN] of int32;
	lt, rt: array[0..MAXN] of int32;
	root, nodesCount: int32;

procedure Update(t: int32);
begin
	if t <> 0 then 
		sz[t] := 1 + sz[lt[t]] + sz[rt[t]];
end;

function NewNode(v: int32): int32;
begin
	inc(nodesCount);
	val[nodesCount] := v;
	prio[nodesCount] := Random(MaxInt);
	sz[nodesCount] := 1;
	lt[nodesCount] := 0;
	rt[nodesCount] := 0;
	Result := nodesCount;
end;

function Merge(l, r: int32): int32;
begin
	if (l = 0) or (r = 0) then
		Result := l + r
	else begin
		if prio[l] > prio[r] then begin
			rt[l] := Merge(rt[l], r);
			Result := l;
		end else begin
			lt[r] := Merge(l, lt[r]);
			Result := r;
		end;
		Update(Result);
	end;
end;

procedure Split(t, v: int32; out l, r: int32);
begin
	if t = 0 then begin
		l := 0; r := 0; {exit;}
	end else begin
		if val[t] < v then begin
			Split(rt[t], v, rt[t], r);
			l := t;
		end else begin
			Split(lt[t], v, l, lt[t]);
			r := t;
		end;
		Update(t);
	end;
end;

// Ažuriran GetAt: Pronalaženje vrednosti na i-toj poziciji (0-indexed)
function GetAt(t, i: int32): int32;
var
	leftSize: int32;
begin
	// U takmičenjima je dobro dodati proveru korena
	if t = 0 then exit(-1); 
	
	leftSize := sz[lt[t]];
	if i < leftSize then
		Result := GetAt(lt[t], i)
	else if i = leftSize then
		Result := val[t]
	else
		Result := GetAt(rt[t], i - leftSize - 1);
end;

procedure Add(v: int32);
var t1, t2: int32;
begin
	Split(root, v, t1, t2);
	root := Merge(Merge(t1, NewNode(v)), t2);
end;

procedure DeleteAt(idx: int32);
var
	t1, t2, t3: int32;
	procedure SplitBySize(t, k: int32; out l, r: int32);
	var curSz: int32;
	begin
		if t = 0 then begin l := 0; r := 0; exit; end;
		curSz := sz[lt[t]];
		if curSz < k then begin
			SplitBySize(rt[t], k - curSz - 1, rt[t], r);
			l := t;
		end else begin
			SplitBySize(lt[t], k, l, lt[t]);
			r := t;
		end;
		Update(t);
	end;
begin
	if (idx < 0) or (idx >= sz[root]) then exit;
	SplitBySize(root, idx, t1, t2);
	SplitBySize(t2, 1, t2, t3);
	root := Merge(t1, t3);
end;

function BisectLeft(t, v: int32): int32;
begin
	if t = 0 then exit(0);
	if val[t] < v then 
		Result := 1 + sz[lt[t]] + BisectLeft(rt[t], v)
	else 
		Result := BisectLeft(lt[t], v);
end;

// Demo korišćenja
var
	i: integer;
begin
	Randomize;
	root := 0; nodesCount := 0;

	// Ubacivanje elemenata
	Add(10); Add(20); Add(5); Add(15);

	writeln('Sortiran niz u stablu:');
	for i := 0 to sz[root] - 1 do
		write(GetAt(root, i), ' '); // 5 10 15 20
	writeln;

	writeln('Prvi indeks za ubacivanje broja 15: ', BisectLeft(root, 15));

	DeleteAt(1); // Briše element na indeksu 1 (broj 10)

	writeln('Nakon brisanja indeksa 1:');
	for i := 0 to sz[root] - 1 do
		write(GetAt(root, i), ' '); // 5 15 20
	writeln;
end.

```
