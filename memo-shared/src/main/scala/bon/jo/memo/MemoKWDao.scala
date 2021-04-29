package bon.jo.memo

trait MemoKWDao extends Dao[Entities.MemoKeywords,Int]{
      def findByKeyWord(kws : String) : FL
}
