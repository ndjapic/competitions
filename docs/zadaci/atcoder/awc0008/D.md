# Задатак: D.pas

```pascal
program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Collections;
const
	nn = 200 * 1000;
type
	THeapComparer<_T> = class
		class function Compare(constref l, r: _T): SizeInt;
	end;
	THeap<_T, _C> = class
	private
		FItems: TList<_T>;
		function Favorite(u: SizeInt): SizeInt;
		function GetItem(Index: SizeInt): _T;
		function GetCount: SizeInt;
	public
		constructor Create;
		destructor Destroy; override;
		procedure Push(Item: _T);
		function Pop: _T;
		property Count: SizeInt read GetCount;
		property Items[Index: SizeInt]: _T read GetItem; default;
	end;
var
	n, i, m: int32;
	ans: int64;
	f, d: array [1 .. nn] of int32;
	pq: THeap<int32, THeapComparer<int32>>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

// THeapComparer<_T>
class function THeapComparer<_T>.Compare(constref l, r: _T): SizeInt;
begin
	Result := - f[l] + f[r];
end;

// THeap<_T, _C>
function THeap<_T, _C>.Favorite(u: SizeInt): SizeInt;
var
	v: SizeInt;
begin
	v := u * 2 + 2;
	if (v >= Count) or (_C.Compare(FItems[v-1], FItems[v]) < 0) then
		dec(v);
	Result := v;
end;

function THeap<_T, _C>.GetItem(Index: SizeInt): _T;
begin
	Result := FItems[Index];
end;

function THeap<_T, _C>.GetCount: SizeInt;
begin
	Result := FItems.Count;
end;

constructor THeap<_T, _C>.Create;
begin
	Inherited Create;
	FItems := TList<_T>.Create;
	{FItems.OwnsObjects := False;}
end;

destructor THeap<_T, _C>.Destroy;
begin
	FItems.Free;
	inherited Destroy;
end;

procedure THeap<_T, _C>.Push(Item: _T);
var
	u, v: SizeInt;
begin
	v := Count;
	FItems.Add(Item);
	u := (v-1) div 2;
	while (v > 0) and (_C.Compare(FItems[v], FItems[u]) < 0) do begin
		FItems.Exchange(u, v);
		v := u;
		u := (v-1) div 2;
	end;
end;

function THeap<_T, _C>.Pop: _T;
var
	u, v: SizeInt;
begin
	Result := FItems[0];
	FItems[0] := FItems[Count - 1];
	// If the Item itself needs to be freed, free it before deleting from the list
	FItems.Delete(Count - 1);

	u := 0;
	v := Favorite(u);
	while (v < Count) and (_C.Compare(FItems[v], FItems[u]) < 0) do begin
		FItems.Exchange(u, v);
		u := v;
		v := Favorite(u);
	end;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	pq := THeap<int32, THeapComparer<int32>>.Create;
	for i := 1 to n do begin
		readln(f[i], d[i]);
		pq.Push(i);
	end;

	ans := 0;
	while (m > 0) and (pq.Count > 0) do begin
		i := pq.Pop;
		inc(ans, f[i]);
		dec(f[i], d[i]);
		if f[i] > 0 then pq.Push(i);
		dec(m);
	end;

	writeln(ans);
	pq.Free;
end.

```
