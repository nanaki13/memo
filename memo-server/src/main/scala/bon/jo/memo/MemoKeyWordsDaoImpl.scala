package bon.jo.memo


import bon.jo.dao.Dao
import bon.jo.dao.Dao.{FB, StringQuery}
import bon.jo.memo.Entities.MemoKeywordRel

import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.{ExecutionContext, Future}

class MemoKeyWordsDaoImpl(implicit val profile: DBProfile.DB,
                          memoDaoImpl: MemoDaoImpl,
                          keyWordDaoImpl: KeyWordDaoImpl) extends DaoImpl with MemoKWDao with  Dao[Entities.MemoKeywords, Int] with StringQuery {

  import profile._
  import profile.profile.api._


  def completeMemo(m : Iterable[Entities.Memo]) = {
    val mAp = m.map((mm)=>mm.id.get -> mm).toMap
    val q = (keyswords join memoKeywords on (_.id === _.idKeyWord)).filter(_._2.idMemo inSet  m.flatMap(_.id)).result
    for (f <- db.run(q)) yield {

        val idOtKw: Map[Int, Seq[Entities.KeyWord]] = f.groupMap(_._2.memo)(_._1)
      mAp.map(e =>Entities.MemoKeywords(e._2, idOtKw.getOrElse(e._1,Nil).toSet))



    }
  }
  override def readAll(a: Iterable[Int])(implicit executionContext: ExecutionContext): FL = {
    ???
  }
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


  def findIdMemoQuery(idKw: Iterable[Int]): Query[Rep[Int], Int, Seq] = for{
    k <- keyswords  if ((k.id inSet   idKw.toList) )
    mk <- memoKeywords if (k.id === mk.idKeyWord  )
  } yield mk.idMemo




  def findIdMemo(idKw: Iterable[Int]) :FIL = db.run(findIdMemoQuery(idKw).result)
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
          val cRel = memoKeywords ++= relation
         // val createAction = DBIO.sequence(cRel)
          db.run(delete).recover{
            case z => println("delete");throw z
          }.flatMap(e => {
            db.run(cRel).map { _ =>
              Some(Entities.MemoKeywords(nMemo, keys))
            }.recover{
              case z => println("createAction");throw z
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
    (for {
      idm <- keyWordDaoImpl.findId(query).map(findIdMemo)
      id <- idm
      a <- memoDaoImpl.readAll(id)
    }yield {
       completeMemo(a)
    }).flatten
  }

  override def find(q: String): FL = findLike(q)

  override def findByKeyWord(kws: String): FL = findLike(kws)
}
