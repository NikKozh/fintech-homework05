package fintech.homework05

import java.time.Instant
import java.util.UUID

import com.sun.net.httpserver.Authenticator

import scala.collection.immutable.HashMap

/**
  * Вам необходимо реализовать api для создания твиттов, получения твитта и лайка твитта
  *
  * Создание твитта:
  * На вход к вам поступает CreateTweetRequest, из которого вы должны создать объект Tweet, обязательно проверив длину
  * текста (а может быть потом появятся и другие проверки).
  * hashTags вычисляется из tweet.text, собирая все слова, начинающиеся с символа `#`.
  * tweet.id генерируется из `UUID.randomUUID.toString`.
  * При условии прохождения всех проверок твит сохраняется в базу данных (вы можете реализовать любой способ сохранения:
  * в памяти, запись в файл, или то, что вам захочется).
  * После выполнения всех операций должен вернуться созданный объект.
  *
  * Получение твитта:
  * На вход к вам поступает GetTweetRequest, вернуть вы должны объект tweet, если он найдем по id.
  *
  * Лайк твитта:
  * Должен обновлять количество лайков у твитта и возвращать новое значение.
  * Если твит не найдет, то должна возвращаться ошибка
  *
  *
  * Все функции должны возвращать значение типа Result[T]
  * в котором может лежать либо текст ошибки, либо результат выполнения
  */

case class Tweet(id:        String,
                 user:      String,
                 text:      String,
                 hashTags:  Seq[String] = Seq.empty,
                 createdAt: Option[Instant] = None,
                 likes:     Int)

case class CreateTweetRequest(text: String, user: String)
case class GetTweetRequest(id: String)
case class LikeRequest(id: String)

trait TweetStorage {
  def saveTweet(tweet: Tweet): Result[Tweet]
  def loadTweet(id: String): Result[Tweet]
  def updateTweet(id:       String,
                  newUser:  Option[String],
                  newText:  Option[String],
                  newLikes: Option[Int]): Result[Tweet]
}

final class InMemoryTweetStorage() extends TweetStorage {
  private var storage: Map[String, Tweet] = new HashMap()

  def saveTweet(tweet: Tweet): Result[Tweet] = storage.get(tweet.id) match {
    case Some(_) => Error(s"Tweet with id ${tweet.id} already exists in the storage!")
    case None =>
      storage = storage.updated(tweet.id, tweet)
      Success(tweet)
  }

  def loadTweet(id: String): Result[Tweet] = storage.get(id) match {
    case Some(tweet) => Success(tweet)
    case None        => Error(s"Tweet with id $id not found in the storage!")
  }

  /*
  * С точки зрения только лишь обновления лайков такой подход к обновлению твита менее эффективный, чем,
  * например, просто инкремент на единицу только в этом классе. Т.к. нам придётся в классе TweetApi сначала
  * запрашивать весь твит по id, чтобы узнать текущее количество лайков, а потом вызывать этот метод.
  * Таким образом, мы открываем соединение с условной "базой данных" и ищем твит два раза вместо одного.
  * Но при этом такой подход намного более расширяемый и гибкий, т.к. позволяет нам как обновлять другие
  * члены класса Tweet, так и обновлять несколько полей за раз. В случае, если требования к классу в будущем
  * изменятся, переписывать старый код и тесты не придётся.
  */
  def updateTweet(id:       String,
                  newUser:  Option[String] = None,
                  newText:  Option[String] = None,
                  newLikes: Option[Int]    = None): Result[Tweet] =
    storage.get(id) match {
      case Some(oldTweet) =>
        val newHashTagsIfNeeds = newText match {
          case Some(text) => TweetApi.getHashTags(text)
          case None       => oldTweet.hashTags
        }
        storage(id) = storage.updated(id, Tweet(oldTweet.id,
                                                newUser.getOrElse(oldTweet.user),
                                                newText.getOrElse(oldTweet.text),
                                                newHashTagsIfNeeds,
                                                oldTweet.createdAt,
                                                newLikes.getOrElse(oldTweet.likes)))
        Success(storage(id))

      case None =>
        Error(s"Tweet with id $id not found in the storage!")
    }
}

class TweetApi(storage: TweetStorage) {
  def createTweet(request: CreateTweetRequest): Result[Tweet] =
    if (request.text.length <= 280)
      storage.saveTweet(Tweet(UUID.randomUUID().toString,
                              request.user,
                              request.text,
                              TweetApi.getHashTags(request.text),
                              Some(Instant.now),
                              0))
    else
      Error("Tweet has more than 280 symbols!")

  def getTweet(request: GetTweetRequest): Result[Tweet] =
    storage.loadTweet(request.id)

  def incrementLikes(request: LikeRequest): Result[Int] =
    storage.loadTweet(request.id) match {
      case Success(tweet) =>
        val newLikes = tweet.likes + 1
        storage.updateTweet(request.id, None, None, Some(newLikes)) match {
          case Success(_)     => Success(newLikes)
          case Error(message) => Error(s"Storage updating error: $message")
        }

      case Error(message) =>
        Error(s"Storage finding error: $message")
    }
}

object TweetApi {
  def getHashTags(text: String): Seq[String] =
    """#([0-9a-zA-Z]+)""".r.findAllIn(text).toList.map(hashTag => hashTag.tail)
}

object TweetApiExample extends App {
  /*
  val storage: TweetStorage = ???
  val app = new TwitterApi(storage)

  val request = CreateTweetRequest(user = "me", text = "Hello, world!")

  val response = app.createTweet(request)
  response match {
    case Success(value) => println(s"Created tweet with id: ${value.id}")
    case Error(message) => println(s"Failed to create tweet: $message")
  }
  */
}
