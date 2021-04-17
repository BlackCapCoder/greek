; ModuleID = 'main.c'
source_filename = "main.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.anon = type { i8 }
%struct.Machine = type { i8, %struct.Tape }
%struct.Tape = type { [65536 x i8], i32 }

@system0 = dso_local local_unnamed_addr constant [6 x %struct.anon] [%struct.anon { i8 11 }, %struct.anon { i8 4 }, %struct.anon { i8 4 }, %struct.anon { i8 13 }, %struct.anon { i8 2 }, %struct.anon { i8 8 }], align 1

; Function Attrs: nounwind sspstrong uwtable
define dso_local zeroext i1 @run() local_unnamed_addr #0 {
  %1 = alloca %struct.Machine, align 4
  %2 = getelementptr inbounds %struct.Machine, %struct.Machine* %1, i64 0, i32 0
  call void @llvm.lifetime.start.p0i8(i64 65544, i8* nonnull %2) #4
  call void @llvm.memset.p0i8.i64(i8* nonnull align 4 dereferenceable(65544) %2, i8 0, i64 65540, i1 false)
  %3 = getelementptr inbounds %struct.Machine, %struct.Machine* %1, i64 0, i32 1, i32 1
  store i32 32768, i32* %3, align 4
  br label %8

4:                                                ; preds = %8
  %5 = icmp eq i64 %19, 0
  %6 = icmp eq i64 %19, 3
  %7 = or i1 %5, %6
  call void @llvm.lifetime.end.p0i8(i64 65544, i8* nonnull %2) #4
  ret i1 %7

8:                                                ; preds = %8, %0
  %9 = phi i32 [ 32768, %0 ], [ %31, %8 ]
  %10 = phi i8 [ 0, %0 ], [ %23, %8 ]
  %11 = phi i32 [ 0, %0 ], [ %32, %8 ]
  %12 = zext i32 %9 to i64
  %13 = icmp ult i32 %9, 65536
  tail call void @llvm.assume(i1 %13) #4
  %14 = getelementptr inbounds %struct.Machine, %struct.Machine* %1, i64 0, i32 1, i32 0, i64 %12
  %15 = load i8, i8* %14, align 1, !tbaa !4
  %16 = zext i8 %15 to i64
  %17 = shl nuw nsw i64 %16, 1
  %18 = zext i8 %10 to i64
  %19 = or i64 %17, %18
  %20 = icmp ult i64 %19, 6
  tail call void @llvm.assume(i1 %20) #4
  %21 = getelementptr inbounds [6 x %struct.anon], [6 x %struct.anon]* @system0, i64 0, i64 %19, i32 0
  %22 = load i8, i8* %21, align 1, !tbaa.struct !7
  %23 = and i8 %22, 1
  %24 = lshr i8 %22, 1
  %25 = and i8 %24, 3
  store i8 %25, i8* %14, align 1, !tbaa !4
  %26 = lshr i64 22, %19
  %27 = and i64 %26, 1
  %28 = icmp eq i64 %27, 0
  %29 = load i32, i32* %3, align 4, !tbaa !10
  %30 = select i1 %28, i32 -1, i32 1
  %31 = add nsw i32 %30, %29
  store i32 %31, i32* %3, align 4, !tbaa !10
  %32 = add nuw nsw i32 %11, 1
  %33 = icmp eq i32 %32, 800000000
  br i1 %33, label %4, label %8
}

; Function Attrs: argmemonly nounwind willreturn
declare void @llvm.lifetime.start.p0i8(i64 immarg, i8* nocapture) #1

; Function Attrs: argmemonly nounwind willreturn writeonly
declare void @llvm.memset.p0i8.i64(i8* nocapture writeonly, i8, i64, i1 immarg) #2

; Function Attrs: argmemonly nounwind willreturn
declare void @llvm.lifetime.end.p0i8(i64 immarg, i8* nocapture) #1

; Function Attrs: nounwind sspstrong uwtable
define dso_local i32 @main() local_unnamed_addr #0 {
  %1 = alloca %struct.Machine, align 4
  %2 = getelementptr inbounds %struct.Machine, %struct.Machine* %1, i64 0, i32 0
  call void @llvm.lifetime.start.p0i8(i64 65544, i8* nonnull %2) #4
  call void @llvm.memset.p0i8.i64(i8* nonnull align 4 dereferenceable(65544) %2, i8 0, i64 65540, i1 false) #4
  %3 = getelementptr inbounds %struct.Machine, %struct.Machine* %1, i64 0, i32 1, i32 1
  store i32 32768, i32* %3, align 4
  br label %4

4:                                                ; preds = %4, %0
  %5 = phi i32 [ 32768, %0 ], [ %26, %4 ]
  %6 = phi i8 [ 0, %0 ], [ %18, %4 ]
  %7 = phi i32 [ 0, %0 ], [ %27, %4 ]
  %8 = zext i32 %5 to i64
  %9 = getelementptr inbounds %struct.Machine, %struct.Machine* %1, i64 0, i32 1, i32 0, i64 %8
  %10 = load i8, i8* %9, align 1, !tbaa !4
  %11 = zext i8 %10 to i64
  %12 = shl nuw nsw i64 %11, 1
  %13 = zext i8 %6 to i64
  %14 = or i64 %12, %13
  %15 = icmp ult i64 %14, 6
  tail call void @llvm.assume(i1 %15) #4
  %16 = getelementptr inbounds [6 x %struct.anon], [6 x %struct.anon]* @system0, i64 0, i64 %14, i32 0
  %17 = load i8, i8* %16, align 1, !tbaa.struct !7
  %18 = and i8 %17, 1
  %19 = lshr i8 %17, 1
  %20 = and i8 %19, 3
  store i8 %20, i8* %9, align 1, !tbaa !4
  %21 = lshr i64 22, %14
  %22 = and i64 %21, 1
  %23 = icmp eq i64 %22, 0
  %24 = load i32, i32* %3, align 4, !tbaa !10
  %25 = select i1 %23, i32 -1, i32 1
  %26 = add nsw i32 %25, %24
  store i32 %26, i32* %3, align 4, !tbaa !10
  %27 = add nuw nsw i32 %7, 1
  %28 = icmp eq i32 %27, 800000000
  br i1 %28, label %29, label %4

29:                                               ; preds = %4
  %30 = icmp eq i64 %14, 0
  %31 = icmp eq i64 %14, 3
  %32 = or i1 %30, %31
  call void @llvm.lifetime.end.p0i8(i64 65544, i8* nonnull %2) #4
  %33 = zext i1 %32 to i32
  ret i32 %33
}

; Function Attrs: nounwind willreturn
declare void @llvm.assume(i1) #3

attributes #0 = { nounwind sspstrong uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="none" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { argmemonly nounwind willreturn }
attributes #2 = { argmemonly nounwind willreturn writeonly }
attributes #3 = { nounwind willreturn }
attributes #4 = { nounwind }

!llvm.module.flags = !{!0, !1, !2}
!llvm.ident = !{!3}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{i32 7, !"PIE Level", i32 2}
!3 = !{!"clang version 11.1.0"}
!4 = !{!5, !5, i64 0}
!5 = !{!"omnipotent char", !6, i64 0}
!6 = !{!"Simple C/C++ TBAA"}
!7 = !{i64 0, i64 1, !8, i64 0, i64 1, !4, i64 0, i64 1, !8}
!8 = !{!9, !9, i64 0}
!9 = !{!"_Bool", !5, i64 0}
!10 = !{!11, !13, i64 65540}
!11 = !{!"", !9, i64 0, !12, i64 4}
!12 = !{!"", !5, i64 0, !13, i64 65536}
!13 = !{!"int", !5, i64 0}
