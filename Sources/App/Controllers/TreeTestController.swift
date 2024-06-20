import Vapor
import Fluent

func clean(string: String) -> String {
    string.replacingOccurrences(of: " ", with: "-")
     .filter { char in
        char.isLetter || char.isNumber
     }
}

struct UserError: DebuggableError, AbortError {
    enum Value {
        case notFound(who: String)
    }
    let value: Value
    var source: ErrorSource?
    var identifier: String {
        switch self.value {
        case .notFound:
            return "userNotFound"
        }
    }
    var reason: String {
        switch self.value {
        case .notFound(let who):
            return "User \(who) was not found"
        }
    }
    var status: HTTPResponseStatus {
        switch self.value {
        case .notFound:
            return .notFound
        }
    }
    init(
        _ value: Value,
        file: String = #file,
        function: String = #function,
        line: UInt = #line,
        column: UInt = #column
    ) {
        self.value = value
        self.source = .init(
            file: file,
            function: function,
            line: line,
            column: column
        )
    }
}

struct StudyError: DebuggableError, AbortError {
    enum Value {
        case notFound(user: UUID, slug: String)
        case notPublished(user: UUID, slug: String)
        case invalidPassword(user: UUID, slug: String)
        case noPermissionToEdit(user: UUID, slug: String)
    }
    let value: Value
    var source: ErrorSource?
    var identifier: String {
        switch self.value {
        case .notFound:
            return "studyNotFound"
        case .notPublished:
            return "studyNotPublished"
        case .invalidPassword:
            return "studyInvalidPassword"
        case .noPermissionToEdit:
            return "studyNoPermissionToEdit"
        }
    }
    var reason: String {
        switch self.value {
        case .notFound(let user, let slug):
            return "Study \(user)/\(slug) was not found"
        case .notPublished(let user, let slug):
            return "Study \(user)/\(slug) was not published"
        case .invalidPassword(let user, let slug):
            return "Invalid password for study \(user)/\(slug)"
        case .noPermissionToEdit(let user, let slug):
            return "No permission to edit study \(user)/\(slug)"
        }
    }
    var status: HTTPResponseStatus {
        switch self.value {
        case .notFound:
            return .notFound
        case .notPublished:
            return .forbidden
        case .invalidPassword:
            return .unauthorized
        case .noPermissionToEdit:
            return .forbidden
        }
    }
    init(
        _ value: Value,
        file: String = #file,
        function: String = #function,
        line: UInt = #line,
        column: UInt = #column
    ) {
        self.value = value
        self.source = .init(
            file: file,
            function: function,
            line: line,
            column: column
        )
    }
}

struct TreeTestError: DebuggableError, AbortError {
    enum Value {
        case taskAnswerMissingInTree(task: String, answer: String)
        case invalidCountOfTasksInSubmission
        case outOfOrderTasksInSubmission
        case submissionAnswerMissingInTree
    }
    let value: Value
    var source: ErrorSource?
    var identifier: String {
        switch self.value {
        case .taskAnswerMissingInTree:
            return "treeTestTaskAnswerMissingInTree"
        case .invalidCountOfTasksInSubmission:
            return "treeTestInvalidCountOfTasksInSubmission"
        case .outOfOrderTasksInSubmission:
            return "treeTestOutOfOrderTasksInSubmission"
        case .submissionAnswerMissingInTree:
            return "treeTestSubmissionAnswerMissingInTree"
        }
    }
    var reason: String {
        switch self.value {
        case .taskAnswerMissingInTree(let task, let answer):
            return "The answer \(answer) for task \(task) is missing in the tree"
        case .invalidCountOfTasksInSubmission:
            return "The submission has an invalid number of completed tasks"
        case .outOfOrderTasksInSubmission:
            return "The submission tasks are out of order"
        case .submissionAnswerMissingInTree:
            return "The submission tasks has answers that are not in the tree"
        }
    }
    var status: HTTPResponseStatus {
        switch self.value {
        case .taskAnswerMissingInTree, .invalidCountOfTasksInSubmission, .outOfOrderTasksInSubmission, .submissionAnswerMissingInTree:
            return .badRequest
        }
    }
    init(
        _ value: Value,
        file: String = #file,
        function: String = #function,
        line: UInt = #line,
        column: UInt = #column
    ) {
        self.value = value
        self.source = .init(
            file: file,
            function: function,
            line: line,
            column: column
        )
    }
}

fileprivate extension Request {
    func getStudy() async throws -> Study {
        let user = self.parameters.get("user")!
        guard let user = try await User.query(on: self.db).filter(\.$username == user).first() else {
            throw UserError(.notFound(who: user))
        }
        let uid = user.id!
        let slug = self.parameters.get("slug")!
        guard let study = try await Study.query(on: self.db)
            .filter(\.$user.$id == uid)
            .filter(\.$slug == slug)
            .filter(\.$kind == .treeTest)
            .with(\.$user)
            .with(\.$treeTestStudy)
            .first() else {
                throw StudyError(.notFound(user: uid, slug: slug))
            }

        return study
    }
}

final class TreeTestController: RouteCollection {
    func boot(routes: RoutesBuilder) throws {
        let tests = routes.grouped("tree-tests")

        // CRUD
        tests.post(":user", use: create)
        tests.get(":user", ":slug", use: getByID)
        tests.patch(":user", ":slug", use: updateByID)

        // get study results
        tests.get(":user", ":slug", "results", use: results)

        // submit an observation (must be published)
        tests.post(":user", ":slug", "completed", use: submitObservation)
    }
    struct TreeTestStudyData: Content {
        var studyData: StudyData
        var tree: TreeNode<TreeStudyItem>
        var tasks: [TreeStudyTask]
    }
    func getByID(req: Request) async throws -> TreeTestStudyData {
        let study = try await req.getStudy()
        let user: User? = req.auth.get()

        if user?.id != study.user.id {
            guard study.published else {
                throw StudyError(.notPublished(user: study.user.id!, slug: study.slug))
            }
            guard study.password == req.headers["StudyPassword"][safe: 0] else {
                throw StudyError(.invalidPassword(user: study.user.id!, slug: study.slug))
            }
        }

        return TreeTestStudyData(
            studyData: study.toData(),
            tree: study.treeTestStudy!.tree,
            tasks: study.treeTestStudy!.tasks
        )
    }
    struct UpdateRequest: Content {
        var tree: TreeNode<TreeStudyItem>
        var tasks: [TreeStudyTask]
    }
    func updateByID(req: Request) async throws -> HTTPStatus {
        let user: User = try req.auth.require()

        let request = try req.content.decode(UpdateRequest.self)
        let study = try await req.getStudy()
        guard user.id == study.user.id else {
            throw StudyError(.noPermissionToEdit(user: study.user.id!, slug: study.slug))
        }

        for task in request.tasks {
            guard request.tree.contains(id: task.answer) else {
                throw TreeTestError(.taskAnswerMissingInTree(task: task.id, answer: task.answer))
            }
        }

        study.treeTestStudy!.tree = request.tree
        study.treeTestStudy!.tasks = request.tasks

        try await study.treeTestStudy!.save(on: req.db)

        return .ok
    }
    struct ResultsResponse: Content {
        var observations: [TreeTestObservationData]
    }
    func results(req: Request) async throws -> ResultsResponse {
        let user: User = try req.auth.require()
        let study = try await req.getStudy()
        guard user.id == study.user.id else {
            throw StudyError(.noPermissionToEdit(user: study.user.id!, slug: study.slug))
        }

        try await study.treeTestStudy!.$observations.load(on: req.db)
        return ResultsResponse(
            observations: study.treeTestStudy!.observations.map
                { TreeTestObservationData(responses: $0.response) }
        )
    }
    struct NewStudyRequest: Content {
        var title: String
    }
    func create(req: Request) async throws -> String {
        let user: User = try req.auth.require()
        let request = try req.content.decode(NewStudyRequest.self)

        return try await req.db.transaction { db in
            let study = Study()
            study.$user.id = user.id!
            study.kind = .treeTest
            study.title = request.title
            study.published = false
            study.slug = clean(string: request.title)
            try await study.create(on: db)

            let treeTest = TreeTestStudy()
            treeTest.tree = TreeNode(
                id: "root",
                content: TreeStudyItem(text: "Root"),
                children: []
            )
            treeTest.tasks = []

            try await study.$treeTestStudy.create(treeTest, on: db)
            return study.slug
        }
    }
    struct SubmitRequest: Content {
        var result: TreeTestObservationData
    }
    func submitObservation(req: Request) async throws -> HTTPStatus {
        let study = try await req.getStudy()
        let treeTest = study.treeTestStudy!
        let request = try req.content.decode(SubmitRequest.self)

        guard study.published else {
            throw StudyError(.notPublished(user: study.user.id!, slug: study.slug))
        }
        guard study.password == req.headers["StudyPassword"][safe: 0] else {
            throw StudyError(.invalidPassword(user: study.user.id!, slug: study.slug))
        }
        guard treeTest.tasks.count == request.result.responses.count else {
            throw TreeTestError(.invalidCountOfTasksInSubmission)
        }
        for (index, element) in request.result.responses.enumerated() {
            guard treeTest.tasks[index].id == element.taskID else {
                throw TreeTestError(.outOfOrderTasksInSubmission)
            }
            guard treeTest.tree.contains(id: element.answer) else {
                throw TreeTestError(.submissionAnswerMissingInTree)
            }
        }

        let observation = TreeTestStudyObservation()
        observation.response = request.result.responses
        try await treeTest.$observations.create(observation, on: req.db)

        return .ok
    }
}
