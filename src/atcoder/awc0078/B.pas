program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #rmq #sparse
uses
	Generics.Collections, 
	Generics.Defaults;
const
	NN = 200 * 1000;
type
	TSparse<T> = class
	private
		FTable: array of array of int32;
		FLg: array of int32;
		FData: array of T;
		FComparer: IComparer<T>;
		procedure PrecomputeLogarithms(Size: int32);
	public
		constructor Create(const Arr: array of T); overload;
		constructor Create(AList: TList<T>); overload;
		function QueryIndex(L, R: int32): int32;
	end;

{ TSparse }

procedure TSparse<T>.PrecomputeLogarithms(Size: int32);
var
	I: int32;
begin
	SetLength(FLg, Size + 1);
	FLg[0] := 0;
	FLg[1] := 0;
	for I := 2 to Size do
		FLg[I] := FLg[I div 2] + 1;
end;

constructor TSparse<T>.Create(const Arr: array of T);
var
	N, MaxLog, I, J: int32;
	Idx1, Idx2: int32;
begin
	N := Length(Arr);
	if N > 0 then begin
		FComparer := TComparer<T>.Default;
		
		SetLength(FData, N);
		for I := 0 to N - 1 do
			FData[I] := Arr[I];

		PrecomputeLogarithms(N);
		MaxLog := FLg[N] + 1;
		SetLength(FTable, N, MaxLog);

		for I := 0 to N - 1 do
			FTable[I, 0] := I;

		for J := 1 to MaxLog - 1 do begin
			I := 0;
			while (I + (1 shl J)) <= N do begin
				Idx1 := FTable[I, J - 1];
				Idx2 := FTable[I + (1 shl (J - 1)), J - 1];

				if FComparer.Compare(FData[Idx1], FData[Idx2]) <= 0 then
					FTable[I, J] := Idx1
				else
					FTable[I, J] := Idx2;

				Inc(I);
			end;
		end;
	end;
end;

constructor TSparse<T>.Create(AList: TList<T>);
begin
	Create(AList.ToArray);
end;

function TSparse<T>.QueryIndex(L, R: int32): int32;
var
	Len, K: int32;
	Idx1, Idx2: int32;
begin
	Len := R - L + 1;
	K := FLg[Len];

	Idx1 := FTable[L, K];
	Idx2 := FTable[R - (1 shl K) + 1, K];

	if FComparer.Compare(FData[Idx1], FData[Idx2]) <= 0 then
		Result := Idx1
	else
		Result := Idx2;
end;

var
	n, i, ai: int32;
	a: TList<int32>;
	RMQ: TSparse<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function dfs(l, r, h: int32): int64;
var
	i, mn: int32;
begin
	if l > r then
		result := 0
	else {if l <= r then} begin
		i := RMQ.QueryIndex(l, r);
		mn := a[i];
		result := mn - h + dfs(l, i-1, mn) + dfs(i+1, r, mn);
	end;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	a := TList<int32>.create;
	for i := 1 to n do begin
		read(ai);
		a.add(ai);
	end;
	readln;

	RMQ := TSparse<int32>.Create(a);
	writeln(dfs(0, n-1, 0));

	a.free;
	RMQ.Free;
end.
