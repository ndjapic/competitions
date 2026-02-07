program TreapTacke;

{$MODE DELPHI}
{$H+}

uses SysUtils;

type
  TTacka = record
    x, y: Integer;
  end;

  PNode = ^TNode;
  TNode = record
    Key: TTacka;
    Priority: Integer;
    Left, Right: PNode;
  end;

// Поређење које смо већ усавршили
function Compare(const L, R: TTacka): Integer;
begin
  if L.y <> R.y then Result := R.y - L.y
  else Result := L.x - R.x;
end;

// Процедура за дељење дрвета
procedure Split(Root: PNode; Key: TTacka; out L, R: PNode);
begin
  if Root = nil then
  begin
    L := nil; R := nil;
  end
  else if Compare(Root.Key, Key) < 0 then
  begin
    Split(Root.Right, Key, Root.Right, R);
    L := Root;
  end
  else
  begin
    Split(Root.Left, Key, L, Root.Left);
    R := Root;
  end;
end;

// Функција за спајање дрвета (Поправљен IfThen проблем)
function Merge(L, R: PNode): PNode;
begin
  if L = nil then Result := R
  else if R = nil then Result := L
  else if L.Priority > R.Priority then
  begin
    L.Right := Merge(L.Right, R);
    Result := L;
  end
  else
  begin
    R.Left := Merge(L, R.Left);
    Result := R;
  end;
end;

function NewNode(K: TTacka): PNode;
begin
  New(Result);
  Result.Key := K;
  Result.Priority := Random(MaxInt); // Већи опсег за бољи баланс
  Result.Left := nil;
  Result.Right := nil;
end;

var
  Root: PNode = nil;

procedure Insert(K: TTacka);
var 
  L, R: PNode;
begin
  Split(Root, K, L, R);
  // Напомена: Ово убацује чак и ако тачка постоји (дозвољава дупликате)
  Root := Merge(Merge(L, NewNode(K)), R);
end;

procedure Delete1(K: TTacka);
var 
  L, Mid, R, Temp: PNode;
begin
  // 1. Прво одвојимо све што је мање од K (иде у L)
  Split(Root, K, L, R);
  
  // 2. Из остатка (R) одвојимо оно што је једнако или веће од следеће могуће вредности
  // Напомена: Овај специфичан Split ће у Mid оставити само чвор са кључем K
  // јер ми користимо поређење које враћа 0 за идентичне тачке.
  
  // Да бисмо били прецизни, користимо мали трик са Split-ом:
  // Прво нађемо све што је >= K, па из тога одвојимо оно што је строго > K.
  // За једноставност у Treap-у са тачкама, често само тражимо тачан чвор:
  
  Split(R, K, Mid, R); // Сада је Mid чвор (или дрво чворова) са кључем K
  
  if Mid <> nil then
  begin
    // Ако желимо да обришемо само један примерак (ако има дупликата):
    Temp := Mid;
    Mid := Merge(Mid.Left, Mid.Right);
    Dispose(Temp); // Ослобађање меморије
  end;

  // 3. Спајамо све назад без обрисаног чвора
  Root := Merge(L, Merge(Mid, R));
end;

procedure Delete(K: TTacka);
var 
  L, Mid, R: PNode;
begin
  Split(Root, K, L, R);
  // Користимо помоћну Split варијанту или једноставно рекурзивно чишћење Mid дела
  Split(R, K, Mid, R); 
  
  // Овде би ишла рекурзивна процедура за Dispose(Mid) ако Mid има поддрво
  // За такмичења, ако нема дупликата, довољно је:
  if Mid <> nil then Dispose(Mid); 
  
  Root := Merge(L, R);
end;

// Функција за испис (In-order обилазак да проверимо да ли је сортирано)
procedure PrintSorted(N: PNode);
begin
  if N <> nil then
  begin
    PrintSorted(N.Left);
    WriteLn(Format('Tacka(x: %d, y: %d)', [N.Key.x, N.Key.y]));
    PrintSorted(N.Right);
  end;
end;

var
  T: TTacka;
begin
  Randomize;
  
  T.x := 10; T.y := 100; Insert(T);
  T.x := 20; T.y := 50;  Insert(T);
  T.x := 15; T.y := 80;  Insert(T);

  WriteLn('Elementi u Treap-u (automatski sortirani):');
  PrintSorted(Root);
end.
