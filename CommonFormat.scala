package com.knoldus.leader_board

import com.knoldus.leader_board.ContributionStatus.ContributionStatus
import com.knoldus.leader_board.ContributionType.ContributionType
import com.knoldus.leader_board.DeliverableType.DeliverableType
import com.knoldus.leader_board.FeedType.FeedType
import com.knoldus.leader_board.ProposalStatus.ProposalStatus
import spray.json.DefaultJsonProtocol
import spray.json.{RootJsonFormat, _}

import java.text.SimpleDateFormat
import java.util.Date
import scala.util.{Failure, Success, Try}

object CommonFormat extends DefaultJsonProtocol {

  def enumJsonFormat[T <: Enumeration](enum: T): RootJsonFormat[T#Value] =
    new RootJsonFormat[T#Value] {
      override def write(t: T#Value): JsValue = JsString(t.toString)

      override def read(json: JsValue): T#Value =
        json match {
          case JsString(string) =>
            Try(enum.withName(string)) match {
              case Success(e) => e
              case Failure(_) => throw DeserializationException(s"Unexpected feature string $string")
            }
          case any => throw DeserializationException(s"Expected to read String type, received $any")
        }
    }

  implicit val ContributionTypeJsonFormat: RootJsonFormat[ContributionType] = enumJsonFormat(ContributionType)
  implicit val StatusTypeJsonFormat: RootJsonFormat[ContributionStatus] = enumJsonFormat(ContributionStatus)
  implicit val ProposalStatusJsonFormat: RootJsonFormat[ProposalStatus] = enumJsonFormat(ProposalStatus)
  implicit val FeedTypeFormat: RootJsonFormat[FeedType] = enumJsonFormat(FeedType)
  implicit val DeliverableContributionTypeJsonFormat: RootJsonFormat[DeliverableType] = enumJsonFormat(DeliverableType)



}


object DateMarshalling {
  implicit object DateFormat extends JsonFormat[Date] {
    def write(date: Date): JsValue = JsString(dateToIsoString(date))
    def read(json: JsValue): Date = json match {
      case JsString(rawDate) =>
        parseIsoDateString(rawDate)
          .fold(deserializationError(s"Expected ISO Date format, got $rawDate"))(identity)
      case error => deserializationError(s"Expected JsString, got $error")
    }
  }

  private val localIsoDateFormatter = new ThreadLocal[SimpleDateFormat] {
    override def initialValue() = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSX")
  }

  private def dateToIsoString(date: Date) =
    localIsoDateFormatter.get().format(date)

  private def parseIsoDateString(date: String): Option[Date] =
    Try{ localIsoDateFormatter.get().parse(date) }.toOption
}
