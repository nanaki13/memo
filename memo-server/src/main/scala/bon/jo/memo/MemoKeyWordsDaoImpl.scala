package bon.jo.memo


import bon.jo.memo.Entities.MemoKeywordRel

import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.Future

class MemoKeyWordsDaoImpl(implicit val profile: DBProfile.DB,
                          memoDaoImpl: MemoDaoImpl,
                          keyWordDaoImpl: KeyWordDaoImpl) extends DaoImpl with Dao[Entities.MemoKeywords, Int] {

  import profile._
  import profile.profile.api._

  override def readAll(limit: Int, offset: Int): FL = {
    (for (a <- (for {
      memos <- memoDaoImpl.readAll(limit, offset)
    } yield {
      for {
        m <- memos
      } yield {
        val q = (keyswords join memoKeywords on (_.id === _.idKeyWord)).filter(_._2.idMemo === m.id).map(_._1).result
        for (f <- db.run(q)) yield Entities.MemoKeywords(m, f.toSet)
      }
    })) yield Future.sequence(a)).flatten
  }


  override def create(a: Entities.MemoKeywords): FO = {
    memoDaoImpl.create(a.memo).flatMap {
      case Some(nMemo) =>
        Future.sequence(a.keyWords.map { keyW =>
          keyW.id match {
            case Some(_) => keyWordDaoImpl.update(keyW)
            case None => keyWordDaoImpl.create(keyW)
          }
        }).flatMap(e => {
          val keys = e.flatten
          val cRel = keys.toSeq.map(k => MemoKeywordRel(nMemo.id.get, k.id.get)).map(memoKeywords += _)
          val ret = DBIO.seq(cRel: _ *)
          val createRelRun = db.run(ret)
          createRelRun.map { _ =>
            Some(Entities.MemoKeywords(nMemo, keys))
          }

        })
      case None => Future.successful(None)
    }
  }

  override def update(a: Entities.MemoKeywords, id: Option[Int]): FO = {
    memoDaoImpl.update(a.memo, id).flatMap {
      case Some(nMemo) =>
        Future.sequence(a.keyWords.map { keyW =>
          keyW.id match {
            case Some(_) => keyWordDaoImpl.update(keyW)
            case None => keyWordDaoImpl.create(keyW)
          }
        }).flatMap(e => {
          val keys = e.flatten
          val relation = keys.toSeq.map(k => MemoKeywordRel(nMemo.id.get, k.id.get))
          val delete = memoKeywords.filter(_.idMemo === a.memo.id).delete
          val cRel = relation.map(memoKeywords += _)
          val createAction = DBIO.sequence(cRel)
          db.run(delete).flatMap(e => {
            db.run(createAction).map { _ =>
              Some(Entities.MemoKeywords(nMemo, keys))
            }
          })

        })
      case None => Future.successful(None)
    }
  }


  override def read(a: Int): FO = {
    val keywWordQuery = keyswords join memoKeywords on (_.id === _.idKeyWord) filter (_._2.idMemo === a) map (_._1)
    val memo = memoDaoImpl.read(a)
    memo flatMap {
      case Some(value) => db.run(keywWordQuery.result).map(ta =>
        Some(Entities.MemoKeywords(value, ta.toSet)))
      case None => Future.successful(None)
    }
  }

  override def delete(a: Int): FB = db run memoKeywords.filter(_.idMemo === a).delete flatMap {
    _ => memoDaoImpl.delete(a)
  }


  def findLike(query: String): FL = {

    def joinQ = keyswords join
      memoKeywords on
      (_.id === _.idKeyWord) join
      memos on (_._2.idMemo === _.id)

    def filter = joinQ filter {
      e => e._1._1.value.like(s"%$query%")
    }

    def mapDistinct = (filter map (_._2)).distinct

    (db.run(mapDistinct.result)) flatMap { memo =>
      Future.sequence {
        for (mm <- memo) yield {
          val q = memoKeywords join keyswords on
            ((m, k) => m.idKeyWord === k.id && m.idMemo === mm.id) map
            (_._2)
          db.run(q.result) map { kws => Entities.MemoKeywords(mm, kws.toSet) }
        }
      }
    }
  }
}
