module Service.Report.ReportMapper where

import Control.Lens ((^.))
import Data.Maybe (catMaybes)

import Api.Resource.Report.ReportDTO
import LensesConfig
import Model.Report.Report

toIndicationDTO :: Indication -> IndicationDTO
toIndicationDTO (AnsweredIndication' i) = AnsweredIndicationDTO' . toAnsweredIndicationDTO $ i

toAnsweredIndicationDTO :: AnsweredIndication -> AnsweredIndicationDTO
toAnsweredIndicationDTO ai =
  AnsweredIndicationDTO
  { _answeredIndicationDTOAnsweredQuestions = ai ^. answeredQuestions
  , _answeredIndicationDTOUnansweredQuestions = ai ^. unansweredQuestions
  }

toMetricSummaryDTO :: MetricSummary -> Maybe MetricSummaryDTO
toMetricSummaryDTO ms =
  case ms ^. measure of
    Just msMeasure ->
      Just MetricSummaryDTO {_metricSummaryDTOMetricUuid = ms ^. metricUuid, _metricSummaryDTOMeasure = msMeasure}
    Nothing -> Nothing

toChapterReportDTO :: ChapterReport -> ChapterReportDTO
toChapterReportDTO chr =
  ChapterReportDTO
  { _chapterReportDTOChapterUuid = chr ^. chapterUuid
  , _chapterReportDTOIndications = toIndicationDTO <$> chr ^. indications
  , _chapterReportDTOMetrics = catMaybes . fmap toMetricSummaryDTO $ chr ^. metrics
  }

toReportDTO :: Report -> ReportDTO
toReportDTO r =
  ReportDTO
  { _reportDTOUuid = r ^. uuid
  , _reportDTOChapterReports = toChapterReportDTO <$> r ^. chapterReports
  , _reportDTOCreatedAt = r ^. createdAt
  , _reportDTOUpdatedAt = r ^. updatedAt
  }
