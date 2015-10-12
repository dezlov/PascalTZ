unit uPascalTZ_Sorters;

{$mode objfpc}{$H+}

{ File: upascaltz_sorters.pas

  This unit contains non optimal implementations of some sort algoritms but
  designed to be easilly injected in any code that needs to sort an array or
  any other data as all compare, swap, etc, actions are handled by callback
  routines "of object". This way is easy to sort an array in your code just
  creating an instance and pointing the:

    OnCompareOfClass
    OnSwapOfClass
    OnCopyToTemporalStorageOfClass
    OnCopyFromTemporalStorageOfClass
    OnCopyElementOfClass

  To functions that perform that basic tasks in your code.
  Not all sorters needs all the handlers, most of them only needs the
  compare and swap routines.

  License: The same as freepascal packages (basically LGPL)

  2009 - José Mejuto.
}

interface

uses
  Classes, SysUtils; 

type
  TSortCompareResult=(eSortCompareLesser=-1,eSortCompareEqual=0,eSortCompareBigger=1);

  TSortCompareFunctionOfClass=function (const AIndex,BIndex: SizeInt): TSortCompareResult of object;
  TSortSwapProcedureOfClass=procedure (const AIndex,BIndex: SizeInt) of object;
  TSortStoreElementProcedureOfClass=procedure (const AIndex,BIndex: SizeInt) of object;

  TSortBinarySearchCheckOfClass=function (Const AIndex: SizeInt): TSortCompareResult of object;

  { TCustomSort }

  TCustomSort=class(TObject)
  private
  protected
    FElements: SizeInt;
    procedure SelfTestCheckRange(const AIndex: SizeInt);
    procedure SelfTestPrepare(); dynamic;
    procedure SelfTestVerify(); dynamic;
    procedure SelfTestCleanup(); dynamic;
  public
    property Elements: SizeInt read FElements;
    procedure SelfTest(); dynamic;
    procedure Sort(); dynamic; abstract;
    constructor Create(const AElements: SizeInt);
  end;

  { TCustomSearch }

  TCustomSearch=class(TObject)
  private
  protected
    FElements: SizeInt;
  public
    function Search(): SizeInt; virtual; abstract;
    Constructor Create(const AElements: SizeInt);
  end;

  { TBinarySearch }

  TBinarySearch=class(TCustomSearch)
  private
  protected
    FCheckElementOfClass: TSortBinarySearchCheckOfClass;
    function CheckElement(const AIndex: SizeInt): TSortCompareResult; virtual;
    function CheckInRange(const AIndex,BIndex: SizeInt): SizeInt; virtual;
  public
    property OnCheckElementOfClass: TSortBinarySearchCheckOfClass read FCheckElementOfClass write FCheckElementOfClass;
    function Search(): SizeInt; override;
  end;

  { TBinarySearchFirstMatch }

  TBinarySearchFirstInList=class(TBinarySearch)
  private
  protected
  public
//    function Search(): SizeInt; override;
  end;

  { TCustomBasicSort }

  TCustomBasicSort=class(TCustomSort)
  private
    SelfTestData: PSizeInt;
    SelfTestTemporal: PSizeInt;
    FOnCompareOfClass: TSortCompareFunctionOfClass;
    FOnSwapOfClass: TSortSwapProcedureOfClass;
    FOnCopyToTemporalOfClass: TSortStoreElementProcedureOfClass;
    FOnCopyFromTemporalOfClass: TSortStoreElementProcedureOfClass;
    FOnCopyElementOfClass: TSortStoreElementProcedureOfClass;
    function SelfTestCompare(const AIndex,BIndex: SizeInt): TSortCompareResult;
    procedure SelfTestSwap(const AIndex,BIndex: SizeInt);
    procedure SelfTestCopyToTemporal(const ATemporalStorageIndex,ASourceIndex: SizeInt);
    procedure SelfTestCopyFromTemporal(const ATemporalStorageIndex,ASourceIndex: SizeInt);
    procedure SelfTestCopyElement(const AToIndex,AFromIndex: SizeInt);
  protected
    function Compare(const AIndex,BIndex: SizeInt): TSortCompareResult; inline;
    procedure Swap(const AIndex,BIndex: SizeInt); inline;
    procedure CopyToTemporalStorage(const ASourceIndex,ATemporalStorageIndex: SizeInt); inline;
    procedure CopyFromTemporalStorage(const ATemporalStorageIndex,ASourceIndex: SizeInt); inline;
    procedure CopyElement(const AToIndex,AFromIndex: SizeInt); inline;
    procedure SelfTestPrepare(); override;
    procedure SelfTestVerify(); override;
    procedure SelfTestCleanup(); override;
  public
    property OnCompareOfClass: TSortCompareFunctionOfClass read FOnCompareOfClass write FOnCompareOfClass;
    property OnSwapOfClass: TSortSwapProcedureOfClass read FOnSwapOfClass write FOnSwapOfClass;
    property OnCopyToTemporalStorageOfClass: TSortStoreElementProcedureOfClass read FOnCopyToTemporalOfClass write FOnCopyToTemporalOfClass;
    property OnCopyFromTemporalStorageOfClass: TSortStoreElementProcedureOfClass read FOnCopyFromTemporalOfClass write FOnCopyFromTemporalOfClass;
    property OnCopyElementOfClass: TSortStoreElementProcedureOfClass read FOnCopyElementOfClass write FOnCopyElementOfClass;
  end;

  { TMergeSort }
  // This kind of sort need a temporal sort space equal to the data
  // to be sort.
  // The logic of this sort is really simple and easy to understand.
  // This is the recursive implementation.
  // Stable: YES.
  // InPlace: NO.
  // Recursive: YES.
  TMergeSort=class(TCustomBasicSort)
  private
  protected
    procedure MergeSort(const ALow,AHigh: SizeInt);
    procedure Merge(const ALow,AMid,AHigh: SizeInt); inline;
    procedure SelfTestPrepare(); override;
    procedure SelfTestCleanup(); override;
  public
    procedure SelfTestVerify(); override;
    procedure Sort(); override;
  end;

  { TMergeIterativeSort }
  // This kind of sort need a temporal sort space equal to the data
  // to be sort.
  // The logic of this sort is really simple and easy to understand.
  // This is the NON-recursive implementation.
  // Stable: YES.
  // InPlace: NO.
  // Recursive: NO.
  TMergeIterativeSort=class(TMergeSort)
  private
  protected
  public
    procedure Sort(); override;
  end;

  { TMergeIterativeOpt1Sort }
  // Same sort as Merge iterative but with swap function to
  // speed up the first pass keeping stability.
  // Stable: YES.
  // InPlace: NO.
  // Recursive: NO.
  // Reference: None.
  TMergeIterativeOpt1Sort=class(TMergeIterativeSort)
  private
  protected
  public
    procedure Sort(); override;
  end;

  { THeapSort }
  // Quite fast sort, this is the choice to many needs.
  // It competes in speed and efficiency with QuickSort, sometimes
  // it gets better results and sometimes the QuickSort is faster.
  // As it is an inplace sort without recursion no extra memory is needed.
  // Stable: NO.
  // InPlace: YES.
  // Recursive: NO.
  // Reference: http://en.wikipedia.org/wiki/Heapsort
  THeapSort=class(TCustomBasicSort)
  private
    procedure Heapify(const Count: SizeInt);
    procedure SiftDown(const AStart, AEnd: SizeInt);
  protected
  public
    procedure Sort(); override;
  end;

  { TGnomeSort }
  // Very simple sort.
  // Stable: YES.
  // InPlace: YES.
  // Recursive: NO.
  // Reference: http://en.wikipedia.org/wiki/Gnome_sort
  TGnomeSort=class(TCustomBasicSort)
  private
  protected
  public
    procedure Sort(); override;
  end;

  { TCombSort }
  // The bubble sort with steroids :) Specially fast when the values to be
  // sorted are heavily repeated, so the entropy is low (many XElement=YElement)
  // and have a "random" distribution.
  // Stable: NO.
  // InPlace: YES.
  // Recursive: NO.
  // Reference: http://en.wikipedia.org/wiki/Comb_sort
  TCombSort11=class(TCustomBasicSort)
  private
  protected
  public
    procedure Sort(); override;
  end;

  { TCocktailSort }
  // The bubble sort bidirectional
  // Stable: YES.
  // InPlace: YES.
  // Recursive: NO.
  // Reference: http://en.wikipedia.org/wiki/Cocktail_sort
  TCocktailSort=class(TCustomBasicSort)
  private
  protected
  public
    procedure Sort(); override;
  end;

  { TQuickSort }
  // The clear example of divide and conquer. Well known and the fastest
  // algorithm for most cases, but with a very bad worst case time (which is rare)
  // Direct compete with HeapSort and MergeSort (this one with a constant
  // worst case time).
  // Stable: NO.
  // InPlace: YES.
  // Recursive: YES.
  // Reference: http://en.wikipedia.org/wiki/Quicksort
  TQuickSort=class(TCustomBasicSort)
  private
  protected
    function SelectPivot(const ALow,AHigh: SizeInt): SizeInt; virtual;
    function DoPartition(const ALow,Ahigh,APivotIndex: SizeInt): SizeInt; virtual;
    procedure QuickSortRecursive(const ALow,AHigh: SizeInt);
  public
    procedure Sort(); override;
  end;

  { TQuickSortThread }
  // Thread object helper to the QuickSortMP experimental implement.
  TQuickSortThread=class(TThread)
  private
  protected
    QS: TQuickSort;
    FLow,FHigh: SizeInt;
  public
    procedure Execute; override;
    Constructor Create(const AParentQS: TQuickSort; const ALow,AHigh: SizeInt);
  end;

  { TQuickSortMP }
  // Basic mutithread implementation of QuickSort (experimental)
  // Stable: NO.
  // InPlace: YES.
  // Recursive: YES.
  // Reference: None
  TQuickSortMP=class(TQuickSort)
  private
  protected
    FMaxThreads: SizeInt;
    FThreads: array of TThread;
    Procedure AfterConstruction; override;
  public
    property MaxThreads: SizeInt read FMaxThreads write FMaxThreads;
    procedure Sort(); override;
  end;

  { TSelectionSort }
  // Basic selection sort implementation, it is being clearly
  // improved in the HeapSort version.
  // Stable: NO.
  // InPlace: YES.
  // Recursive: NO.
  // Reference: http://en.wikipedia.org/wiki/Selection_sort
  TSelectionSort=class(TCustomBasicSort)
  private
  protected
  public
    procedure Sort(); override;
  end;

  { TBubleSort }
  // The classic inital sort algo. Not efficient at all.
  // Stable: YES.
  // InPlace: YES.
  // Recursive: NO.
  // Reference: http://en.wikipedia.org/wiki/Selection_sort
  TBubleSort=class(TCustomBasicSort)
  private
  protected
  public
    procedure Sort(); override;
  end;

  { TInsertionSort }
  // It can not be implemented using custombasicsort API as
  // it needs compares with a given value outside the current
  // data array. So any insertion sort is not implemented.
  // Reference: http://en.wikipedia.org/wiki/Insertion_sort
  //TInsertionSort=class(TCustomBasicSort);

(*  { TSmoothSort }
  // Complex advanced version of HeapSort. It performs better with
  // partially presorted data. It needs similar function as insertion
  // sort, so it is not implemented by now.
  // Stable: NO.
  // InPlace: YES.
  // Recursive: NO.
  // Reference: http://www.cs.utexas.edu/~EWD/transcriptions/EWD07xx/EWD796a.html
  // Reference: http://en.wikibooks.org/wiki/Algorithm_implementation/Sorting/Smoothsort
  TSmoothSort=class(TCustomBasicSort)
  private
  protected
    procedure Up(var vb,vc: SizeInt);
    procedure Down(var vb,vc: SizeInt);
    procedure Sift(const r1: SizeInt);
  public
    procedure Sort(); override;
  end;
*)

implementation

{ TCustomSort }

procedure TCustomSort.SelfTestCheckRange(const AIndex: SizeInt);
begin
  If (AIndex<0) or (AIndex>(FElements-1)) then begin
    Raise ERangeError.CreateFmt('Trying to access out of range 0-%d (%d)',[FElements-1,AIndex]);
  end;
end;

procedure TCustomSort.SelfTestPrepare();
begin
  //Do nothing
end;

procedure TCustomSort.SelfTestVerify();
begin
  //Do nothing
end;

procedure TCustomSort.SelfTestCleanup();
begin
  //Do nothing
end;

procedure TCustomSort.SelfTest();
begin
  SelfTestPrepare();
  try
    Sort();
    SelfTestVerify();
  finally
    SelfTestCleanup();
  end;
end;

constructor TCustomSort.Create(const AElements: SizeInt);
begin
  FElements:=AElements;
end;

{ TCustomBasicSort }

function TCustomBasicSort.SelfTestCompare(const AIndex, BIndex: SizeInt
  ): TSortCompareResult;
begin
  SelfTestCheckRange(AIndex);
  SelfTestCheckRange(BIndex);
  if SelfTestData[AIndex*2]>SelfTestData[BIndex*2] then begin
    Result:=eSortCompareBigger;
  end else if SelfTestData[AIndex*2]<SelfTestData[BIndex*2] then begin
    Result:=eSortCompareLesser;
  end else begin
    Result:=eSortCompareEqual;
  end;
end;

procedure TCustomBasicSort.SelfTestSwap(const AIndex, BIndex: SizeInt);
var
  Temporal: SizeInt;
begin
  SelfTestCheckRange(AIndex);
  SelfTestCheckRange(BIndex);
  Temporal:=SelfTestData[AIndex*2];
  SelfTestData[AIndex*2]:=SelfTestData[BIndex*2];
  SelfTestData[BIndex*2]:=Temporal;
  //Swap the stability control too...
  Temporal:=SelfTestData[AIndex*2+1];
  SelfTestData[AIndex*2+1]:=SelfTestData[BIndex*2+1];
  SelfTestData[BIndex*2+1]:=Temporal;
end;

procedure TCustomBasicSort.Swap(const AIndex, BIndex: SizeInt); inline;
begin
  if FOnSwapOfClass<>nil then
    FOnSwapOfClass(AIndex,BIndex)
  else
    Raise Exception.Create('Missing Swap event assignement');
end;

function TCustomBasicSort.Compare(const AIndex, BIndex: SizeInt
  ): TSortCompareResult; inline;
begin
  if FOnCompareOfClass<>nil then begin
    Result:=FOnCompareOfClass(AIndex,BIndex);
    Exit;
  end;
  Raise Exception.Create('Missing Compare event assignement');
end;

procedure TCustomBasicSort.SelfTestPrepare();
var
  j: integer;
begin
  inherited SelfTestPrepare;
  FOnCompareOfClass:=@SelfTestCompare;
  FOnSwapOfClass:=@SelfTestSwap;
  FOnCopyToTemporalOfClass:=@SelfTestCopyToTemporal;
  FOnCopyFromTemporalOfClass:=@SelfTestCopyFromTemporal;
  GetMem(SelfTestData,(sizeof(SizeUInt)*FElements)*2); //Value + Stability check
  Randomize;
  j:=0;
  while j<FElements do begin
    SelfTestData[j*2]:=Random(High(SizeInt));
    SelfTestData[(j*2)+1]:=j;
    inc(j);
  end;
end;

procedure TCustomBasicSort.SelfTestVerify();
var
  j: SizeInt;
begin
  j:=0;
  while j<FElements-1 do begin
    if Compare(j,j+1)=eSortCompareBigger then begin
      Raise Exception.CreateFmt('Sort selftest failed at element %d. [%s]',[j,Self.ClassName]);
    end;
    inc(j,2);
  end;
end;

procedure TCustomBasicSort.SelfTestCleanup();
begin
  FreeMem(SelfTestData);
  SelfTestData:=nil;
  Inherited SelfTestCleanup;
end;

{ THeapSort }

procedure THeapSort.Heapify(const Count: SizeInt);
var
  Start: SizeInt;
begin
  (*start is assigned the index in a of the last parent node*)
  Start := (Count - 2) div 2;

  while Start >= 0 do begin
       (*sift down the node at index start to the proper place such that all nodes below
        the start index are in heap order*)
       SiftDown(start, Count-1);
       Dec(Start);
       (*after sifting down the root all nodes/elements are in heap order*)
  end;
end;

procedure THeapSort.SiftDown(const AStart, AEnd: SizeInt);
var
  Root: SizeInt;
  Child: SizeInt;
begin
  Root := AStart;

  while (Root * 2 + 1) <= AEnd do begin //While the root has at least one child
    Child := Root * 2 + 1;              //root*2+1 points to the left child
    (*If the child has a sibling and the child's value is less than its sibling's...*)
    if ((Child + 1) <= AEnd) and (Compare(child,child + 1)=eSortCompareLesser) then
      inc(Child);                       //... then point to the right child instead)
    if Compare(root,child)=eSortCompareLesser then begin //(out of max-heap order)
      Swap(root,child);
      Root := Child                     //(repeat to continue sifting down the child now)
    end else
       break;
  end;
end;

procedure THeapSort.Sort();
var
  LEnd: SizeInt;
begin
  Heapify(FElements);
  LEnd := FElements - 1;
  while LEnd > 0 do begin
    //swap the root(maximum value) of the heap with the last element of the heap
    Swap(Lend,0);
    //decrease the size of the heap by one so that the previous max value will
    //stay in its proper placement
    LEnd := LEnd - 1;
    //put the heap back in max-heap order
    SiftDown(0, LEnd);
  end;
end;

{ TGnomeSort }

procedure TGnomeSort.Sort();
var
  j,i: SizeInt;
begin
  i := 1;
  j := 2;
  while i < FElements do begin
    if Compare(i-1,i)<>eSortCompareBigger then begin // for descending sort, reverse the comparison to >=
      i := j ;
      Inc(j);
    end else begin
      Swap(i-1,i);
      Dec(i);
      if i = 0 then i := 1;
    end;
 end;
end;

{ TCustomMergeSort }

procedure TCustomBasicSort.SelfTestCopyToTemporal(const ATemporalStorageIndex,
  ASourceIndex: SizeInt);
begin
  SelfTestCheckRange(ATemporalStorageIndex);
  SelfTestCheckRange(ASourceIndex);
  SelfTestTemporal[ATemporalStorageIndex*2]:=SelfTestData[ASourceIndex*2];
  //Copy the stability controller too...
  SelfTestTemporal[ATemporalStorageIndex*2+1]:=SelfTestData[ASourceIndex*2+1];
end;

procedure TCustomBasicSort.SelfTestCopyFromTemporal(const ATemporalStorageIndex,
  ASourceIndex: SizeInt);
begin
  SelfTestCheckRange(ATemporalStorageIndex);
  SelfTestCheckRange(ASourceIndex);
  //Copy the stability controller too...
  SelfTestData[ASourceIndex*2]:=SelfTestTemporal[ATemporalStorageIndex*2];
  SelfTestData[ASourceIndex*2+1]:=SelfTestTemporal[ATemporalStorageIndex*2+1];
end;

procedure TCustomBasicSort.SelfTestCopyElement(const AToIndex,
  AFromIndex: SizeInt);
begin
  SelfTestCheckRange(AToIndex);
  SelfTestCheckRange(AFromIndex);
  SelfTestData[AToIndex*2]:=SelfTestTemporal[AFromIndex*2];
  SelfTestData[AToIndex*2+1]:=SelfTestTemporal[AFromIndex*2+1];
end;

procedure TCustomBasicSort.CopyToTemporalStorage(const ASourceIndex,
  ATemporalStorageIndex: SizeInt); inline;
begin
  SelfTestCheckRange(ATemporalStorageIndex);
  SelfTestCheckRange(ASourceIndex);
  if FOnCopyToTemporalOfClass<>nil then
    FOnCopyToTemporalOfClass(ASourceIndex,ATemporalStorageIndex)
  else
    Raise Exception.Create('Missing CopyToTemporal event assignement');
end;

procedure TCustomBasicSort.CopyFromTemporalStorage(const ATemporalStorageIndex,
  ASourceIndex: SizeInt); inline;
begin
  if FOnCopyFromTemporalOfClass<>nil then
    FOnCopyFromTemporalOfClass(ATemporalStorageIndex,ASourceIndex)
  else
    Raise Exception.Create('Missing CopyFromTemporal event assignement');
end;

procedure TCustomBasicSort.CopyElement(const AToIndex, AFromIndex: SizeInt);
  inline;
begin
  if FOnCopyElementOfClass<>nil then
    FOnCopyElementOfClass(AToIndex,AFromIndex)
  else
    Raise Exception.Create('Missing CopyElement event assignement');
end;

{ TMergeSort }

procedure TMergeSort.MergeSort(const ALow, AHigh: SizeInt);
var
  CutPoint: SizeInt;
begin
  if ALow <> AHigh then begin
  	CutPoint:=(ALow + AHigh) div 2;
    MergeSort(ALow,CutPoint);
    MergeSort(CutPoint + 1, AHigh);
    Merge(ALow, CutPoint, AHigh);
  end;
end;

procedure TMergeSort.Merge(const ALow, AMid, AHigh: SizeInt);
var
  LowHalf,HighHalf,Count: SizeInt;
begin
  Count:=ALow;
	LowHalf:= ALow;
	HighHalf:=AMid + 1;
	while (LowHalf <= AMid) and (HighHalf <= AHigh) do begin
    if Compare(HighHalf, LowHalf) = eSortCompareLesser Then begin
      CopyToTemporalStorage(Count,HighHalf);
      inc(HighHalf);
    end else begin
      CopyToTemporalStorage(Count,LowHalf);
      inc(LowHalf);
    end;
    inc(Count);
  end;
  while LowHalf <= AMid do begin
    CopyToTemporalStorage(Count,LowHalf);
    inc(LowHalf);
    inc(Count);
  end;
	while HighHalf <= AHigh do begin
    CopyToTemporalStorage(Count,HighHalf);
    inc(HighHalf);
    inc(Count);
  end;
  for Count := ALow to AHigh do begin
    CopyFromTemporalStorage(Count,Count);
  end;
end;

procedure TMergeSort.SelfTestPrepare();
begin
  inherited SelfTestPrepare();
  GetMem(SelfTestTemporal,(sizeof(SizeUInt)*FElements)*2);
end;

procedure TMergeSort.SelfTestCleanup();
begin
  FreeMem(SelfTestTemporal);
  inherited SelfTestCleanup();
end;

procedure TMergeSort.SelfTestVerify();
var
  j: SizeInt;
  Prev,Next: SizeInt;
begin
  inherited SelfTestVerify();
  //Now check stability of the sort...
  j:=0;
  while j<FElements-1 do begin
    if Compare(j,j+1)=eSortCompareEqual then begin
      //If they are equal, this and next original position must be
      //ordered minor to major...
      Prev:=SelfTestData[j*2+1];
      Next:=SelfTestData[(j+1)*2+1];
      if Prev>=Next then begin
        Raise ERangeError.CreateFmt('Stability test failed at element %d',[j]);
      end;
    end;
    inc(j,2);
  end;
end;

procedure TMergeSort.Sort();
begin
  MergeSort(0,FElements-1);
end;

{ TMergeIterativeSort }

procedure TMergeIterativeSort.Sort();
var
  ALow,AHigh,CutPoint: SizeInt;
  Stepper,StepperLimit: SizeInt;
begin
  Stepper:=2;
  StepperLimit:=(FElements-1)*2;
  while Stepper<StepperLimit do begin
    ALow:=0;
    While ALow<FElements do begin
      AHigh:=ALow+Stepper-1;
      CutPoint:=(ALow+AHigh) div 2;
      if AHigh>=FElements then begin
        AHigh:=FElements-1;
        if CutPoint>=FElements then begin
          CutPoint:=AHigh;
        end;
      end;
      Merge(ALow,CutPoint,AHigh);
      ALow:=AHigh+1;
    end;
    Stepper:=Stepper * 2;
  end;
end;

{ TMergeIterativeOpt1Sort }

procedure TMergeIterativeOpt1Sort.Sort();
var
  ALow,AHigh,CutPoint: SizeInt;
  Stepper: SizeInt;
  StepperLimit: SizeInt;
begin
  //First pass (stepper=2) using swap.
  Stepper:=2;
  ALow:=0;
  AHigh:=FElements-1;
  While ALow<AHigh do begin
    if Compare(ALow,ALow+1)=eSortCompareBigger then begin
      Swap(ALow,ALow+1);
    end;
    inc(ALow,Stepper);
  end;
  //Now the normal iterative mode...
  Stepper:=4;
  StepperLimit:=(FElements-1)*2;
  while Stepper<StepperLimit do begin
    ALow:=0;
    While ALow<FElements do begin
      AHigh:=ALow+Stepper-1;
      CutPoint:=(ALow+AHigh) div 2;
      if AHigh>=FElements then begin
        AHigh:=FElements-1;
        if CutPoint>=FElements then begin
          CutPoint:=AHigh;
        end;
      end;
      Merge(ALow,CutPoint,AHigh);
      ALow:=AHigh+1;
    end;
    Stepper:=Stepper * 2;
  end;
end;

{ TCombSort11 }

procedure TCombSort11.Sort();
var
  Gap,Swaps: SizeInt;
  i: SizeInt;
begin
  Gap:=FElements; //initialize gap size
  Swaps:=0;
  While not ((Gap<=1) and (Swaps=0)) do begin
    //update the gap value for a next comb
    if gap > 1 then begin
      gap :=trunc(gap / 1.3);
      if (gap = 10) or (gap = 9) then begin
        gap := 11;
      end;
    end;

    i := 0;
    swaps := 0; //see bubblesort for an explanation

    //a single "comb" over the input list
    while not ((i + Gap) >= FElements) do begin //see shellsort for similar idea
      if Compare(i,i+gap)=eSortCompareBigger then begin
        Swap(i,i+gap);
        Swaps := 1 // Arithmetic_overflow fixup
      end;
      inc(i);
    end;
  end;
end;

{ TCocktailSort }

procedure TCocktailSort.Sort();
var
  LBegin,LEnd: SizeInt;
  Swapped: Boolean;
  i: SizeInt;
begin
  LBegin := -1;
  LEnd := FElements - 2;
  repeat
    Swapped:=false;
    // increases `begin` because the elements before `begin` are in correct order
    Inc(LBegin);
    for i := LBegin to LEnd do begin
      if Compare(i,i+1)=eSortCompareBigger then begin
        Swap(i,i+1);
        Swapped:=true;
      end;
    end;
    if not Swapped then break; //We can go out, no swap, no changes...
    Swapped:=false;
    // decreases `end` because the elements after `end` are in correct order
    Dec(LEnd);
    for i := LEnd Downto LBegin do begin
      if Compare(i,i+1)=eSortCompareBigger then begin
        Swap(i,i+1);
        swapped:=true;
      end;
    end;
  until not Swapped;
end;

{ TQuickSort }

function TQuickSort.SelectPivot(const ALow, AHigh: SizeInt): SizeInt;
begin
  //The basic quick sort always use the element in the middle between
  //ALow and AHigh whichever one it is, which could render in a very bad
  //partition element.
  Result:=(ALow+AHigh) div 2;
end;

function TQuickSort.DoPartition(const ALow, AHigh, APivotIndex: SizeInt): SizeInt;
var
  StoreIndex: SizeInt;
  i: SizeInt;
begin
  Swap(APivotIndex,AHigh); // Move pivot to end
  storeIndex := ALow;
  for i := ALow to AHigh-1 do begin
    if Compare(i,AHigh)=eSortCompareLesser then begin
      Swap(i,StoreIndex);
      inc(StoreIndex);
    end;
  end;
  Swap(StoreIndex,AHigh); // Move pivot to its final place
  Result:=StoreIndex;
end;

procedure TQuickSort.QuickSortRecursive(const ALow, AHigh: SizeInt);
var
  PivotIndex: SizeInt;
begin
  if AHigh > ALow then begin
    PivotIndex:=SelectPivot(ALow,AHigh);
    PivotIndex := DoPartition(ALow, AHigh, PivotIndex);
    QuickSortRecursive(ALow, PivotIndex - 1);
    QuickSortRecursive(PivotIndex + 1, AHigh);
  end;
end;

procedure TQuickSort.Sort();
begin
  QuickSortRecursive(0,FElements-1);
end;

{ TQuickSortMP }

procedure TQuickSortMP.AfterConstruction;
begin
  inherited AfterConstruction;
  FMaxThreads:=2;
end;

procedure TQuickSortMP.Sort();
var
  j: integer;
  ReduceThreads,Rounds: SizeInt;
  PivotIndex: SizeInt;
begin
  //FMaxThread must be 1,2,4,8,16,32,64....
  if FMaxThreads=0 Then FMaxThreads:=1;
  ReduceThreads:=FMaxThreads;
  Rounds:=0;
  while ReduceThreads>0 do begin
    inc(Rounds);
    ReduceThreads:=ReduceThreads shr 1;
  end;
  FMaxThreads:=1 shl (Rounds-1);
  //Currently it only handles 2 threads, as it is only an
  //experiment.
  FMaxThreads:=2;

  SetLength(FThreads,FMaxThreads);
  j:=0;
  PivotIndex:=SelectPivot(0,FElements-1);
  PivotIndex := DoPartition(0, FElements-1, PivotIndex);
  FThreads[0]:=TQuickSortThread.Create(Self,0,PivotIndex);
  FThreads[1]:=TQuickSortThread.Create(Self,PivotIndex+1,FElements-1);

  j:=0;
  //Wait for all threads.
  while j < MaxThreads do begin
    FThreads[j].WaitFor;
    FThreads[j].Free;
    inc(j);
  end;
end;

{ TQuickSortThread }

procedure TQuickSortThread.Execute;
begin
  QS.QuickSortRecursive(FLow,FHigh);
end;

constructor TQuickSortThread.Create(const AParentQS: TQuickSort; const ALow,
  AHigh: SizeInt);
begin
  Self.FreeOnTerminate:=false;
  QS:=AParentQS;
  FLow:=ALow;
  FHigh:=AHigh;
  //Thread will be launched here
  inherited Create(false,0); //Not suspended create...
end;

{ TCustomSearch }

constructor TCustomSearch.Create(const AElements: SizeInt);
begin
  FElements:=AElements;
end;

{ TBinarySearch }

function TBinarySearch.CheckElement(const AIndex: SizeInt
  ): TSortCompareResult;
begin
  if FCheckElementOfClass<>nil then begin
    Result:=FCheckElementOfClass(Aindex);
  end else
    Raise Exception.Create('Missing CheckElement event assignement');
end;

function TBinarySearch.CheckInRange(const AIndex, BIndex: SizeInt): SizeInt;
var
  r: TSortCompareResult;
  Pivot: SizeInt;
begin
  if AIndex<=BIndex Then begin
    Pivot:=(AIndex+BIndex) div 2;
    r:=CheckElement(Pivot);
    if r=eSortCompareEqual then begin
      Result:=Pivot;
    end else if r=eSortCompareBigger then begin
      Result:=CheckInRange(Pivot+1,BIndex);
    end else if r=eSortCompareLesser then begin
      Result:=CheckInRange(AIndex,Pivot-1);
    end;
  end else begin
    Result:=-1;
  end;
end;

function TBinarySearch.Search(): SizeInt;
begin
  Result:=CheckInRange(0,FElements-1);
end;

{ TSelectionSort }

procedure TSelectionSort.Sort();
var
  i,j,s: SizeInt;
begin
  for i := 0 to FElements-1 do begin
    s:=i;
    for j := i+1 to FElements-1 do begin
      if Compare(j,s)=eSortCompareLesser then begin
        s:=j;
      end;
    end;
    Swap(S,I);
  end;
end;

{ TBubleSort }

procedure TBubleSort.Sort();
var
  j: SizeInt;
  Swapped: Boolean;
begin
  repeat
    Swapped:=false;
    for j := 0 to FElements-2 do begin
      if Compare(j,j+1)=eSortCompareBigger Then begin
        Swap(j,j+1);
        Swapped:=true;
      end;
    end;
  until not swapped;
end;

end.

